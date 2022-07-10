package org.sba_research.worker

import akka.actor.typed._
import akka.actor.typed.delivery.ConsumerController
import akka.actor.typed.pubsub.Topic
import akka.actor.typed.scaladsl._
import org.sba_research.utils.ValidationReport
import org.sba_research.worker.WorkExecutor.ExecuteWork

/**
  * The worker is actually more of a middle manager, delegating the actual work
  * to the WorkExecutor, supervising it and keeping itself available to interact with the work manager.
  * cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala
  */
object Worker {

  sealed trait Message

  case class DeliveredMessage(confirmTo: ActorRef[ConsumerController.Confirmed], message: WorkManager.WorkerCommand, seqNr: Long) extends Message

  sealed trait WorkResultValue

  case class ValidationWorkResult(validationReport: ValidationReport) extends WorkResultValue

  case class WorkComplete(work: Work, result: Option[WorkResultValue] = None) extends Message

  case class WorkFailed(id: String, t: Throwable) extends Message

  case class WorkTimeout() extends Message

  def apply(resultTopicActor: ActorRef[Topic.Command[WorkFinished]],
            workExecutorFactory: () => Behavior[ExecuteWork] = () => WorkExecutor()): Behavior[Message] =
    Behaviors.setup[Message] { ctx =>
      val consumerController = ctx.spawn(ConsumerController[WorkManager.WorkerCommand](WorkManager.ManagerServiceKey), "consumer-controller")
      val deliverAdapter = ctx.messageAdapter[ConsumerController.Delivery[WorkManager.WorkerCommand]](d => DeliveredMessage(d.confirmTo, d.message, d.seqNr))
      consumerController ! ConsumerController.Start(deliverAdapter)
      Behaviors
        .supervise(new Worker(ctx, workExecutorFactory, resultTopicActor).idle())
        .onFailure[Exception](SupervisorStrategy.restart)
    }

}

class Worker private(
                      ctx: ActorContext[Worker.Message],
                      workExecutorFactory: () => Behavior[ExecuteWork],
                      resultTopicActor: ActorRef[Topic.Command[WorkFinished]]) {

  import Worker._

  def createWorkExecutor(): ActorRef[ExecuteWork] = {
    val supervised = Behaviors.supervise(workExecutorFactory()).onFailure[Exception](SupervisorStrategy.stop)
    val ref = ctx.spawn(supervised, "work-executor")
    ctx.watch(ref)
    ref
  }

  def idle(workExecutor: ActorRef[ExecuteWork] = createWorkExecutor()): Behavior[Worker.Message] =
    Behaviors.receiveMessagePartial[Worker.Message] {
      case DeliveredMessage(confirmTo, message, _) =>
        ctx.watch(confirmTo)
        message match {
          case WorkManager.DoWork(w@WorkCreateRemoteDataset(_, _)) =>
            ctx.log.info("Got work (create remote dataset): {}", w)
            workExecutor ! WorkExecutor.CreateRemoteDataset(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkPushModelToRemoteDataset(_, _, amlFilePath, sfcFilePath)) =>
            ctx.log.info("Got work (push model to remote dataset): {}", w)
            workExecutor ! WorkExecutor.PushModelToRemoteDataset(w, ctx.self, amlFilePath, sfcFilePath)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkAugmentModel(_, _, doAMLqual)) =>
            ctx.log.info("Got work (augment model): {}", w)
            workExecutor ! WorkExecutor.AugmentModel(w, ctx.self, doAMLqual)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkValidateEngineeringData(_, _, _)) =>
            ctx.log.info("Got work (validate engineering data): {}", w)
            workExecutor ! WorkExecutor.ValidateEngineeringData(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkValidateWithSecurityRule(_, _, _)) =>
            ctx.log.info("Got work (validate with security rule): {}", w)
            workExecutor ! WorkExecutor.ValidateWithSecurityRule(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkCveCheck(_, _)) =>
            ctx.log.info("Got work (perform CVE check): {}", w)
            workExecutor ! WorkExecutor.PerformCveCheck(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkInstantiateVulnerabilities(_, _, _)) =>
            ctx.log.info("Got work (instantiate vulnerabilities): {}", w)
            workExecutor ! WorkExecutor.InstantiateVulnerabilities(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkGenerateAttackGraph(_, _)) =>
            ctx.log.info("Got work (generate attack graph): {}", w)
            workExecutor ! WorkExecutor.GenerateAttackGraph(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkGenerateQOPN(_, _)) =>
            ctx.log.info("Got work (generate QOPN): {}", w)
            workExecutor ! WorkExecutor.GenerateQOPN(w, ctx.self)
            working(workExecutor, confirmTo)
          case WorkManager.DoWork(w@WorkExecuteCaseStudy(_, _)) =>
            ctx.log.info("Got work (execute case study): {}", w)
            workExecutor ! WorkExecutor.ExecuteCaseStudy(w, ctx.self)
            working(workExecutor, confirmTo)
          case _ => Behaviors.unhandled
        }
    }.receiveSignal {
      case (context, Terminated(ref)) =>
        context.log.info("WorkManager stopped: {}", ref.path.name)
        Behaviors.same
    }

  def working(workExecutor: ActorRef[ExecuteWork], confirmTo: ActorRef[ConsumerController.Confirmed]): Behavior[Worker.Message] =
    Behaviors
      .receiveMessagePartial[Worker.Message] {
        case Worker.WorkComplete(work, result) =>
          ctx.log.info("Work '{}' is complete. Result: '{}'", work, result)
          confirmTo ! ConsumerController.Confirmed
          work match {
            case _: WorkCreateRemoteDataset =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, FusekiDatasetCreationSuccessful))
            case _: WorkPushModelToRemoteDataset =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, FusekiPushModelToRemoteDatasetSuccessful))
            case _: WorkAugmentModel =>
              /* During the augmentation step, we perform validity check via the reasoner.
              * Thus, if the model is not valid, we receive a validation work result. */
              result match {
                case Some(r: ValidationWorkResult) =>
                  resultTopicActor ! Topic.Publish(WorkFinished(work, ValidationOfEngineeringDataSuccessful(r.validationReport)))
                case _ => resultTopicActor ! Topic.Publish(WorkFinished(work, ModelAugmentationSuccessful))
              }
            case _: WorkValidateEngineeringData =>
              result match {
                case Some(r: ValidationWorkResult) =>
                  resultTopicActor ! Topic.Publish(WorkFinished(work, ValidationOfEngineeringDataSuccessful(r.validationReport)))
                case _ =>
                  ctx.log.error("Received wrong result type for validation of engineering data work item ({}).", result)
              }
            case _: WorkValidateWithSecurityRule =>
              result match {
                case Some(r: ValidationWorkResult) =>
                  resultTopicActor ! Topic.Publish(WorkFinished(work, ValidationWithSecurityRuleSuccessful(r.validationReport)))
                case _ =>
                  ctx.log.error("Received wrong result type for validation with security rule work item ({}).", result)
              }
            case _: WorkCveCheck =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, CveCheckSuccessful))
            case _: WorkInstantiateVulnerabilities =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, InstantiationOfVulnerabilitiesSuccessful))
            case _: WorkGenerateAttackGraph =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, GenerationOfAttackGraphSuccessful))
            case _: WorkGenerateQOPN =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, GenerationOfQOPNSuccessful))
            case _: WorkExecuteCaseStudy =>
              resultTopicActor ! Topic.Publish(WorkFinished(work, ExecutionOfCaseStudySuccessful))
            case _ =>
          }
          idle(workExecutor)
        case Worker.WorkFailed(s, e) =>
          ctx.log.error(s, e)
          throw e
        case _: DeliveredMessage =>
          ctx.log.warn("Yikes. Reliable delivery told me to do work, while I'm already working.")
          Behaviors.unhandled

      }
      .receiveSignal {
        case (_, Terminated(_)) =>
          ctx.log.info("Work executor terminated")
          // The work is confirmed meaning it won't be re-delivered. Sending back a failure would need
          // to be done explicitly
          confirmTo ! ConsumerController.Confirmed
          idle(createWorkExecutor())
      }

}
