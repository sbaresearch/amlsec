package org.sba_research.worker

import akka.Done
import akka.actor.typed.delivery.WorkPullingProducerController.{MessageWithConfirmation, RequestNext}
import akka.actor.typed.delivery.{ConsumerController, WorkPullingProducerController}
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.pubsub.Topic
import akka.persistence.typed.{PersistenceId, RecoveryCompleted}
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.util.Timeout
import org.sba_research.worker.WorkState.{WorkAccepted, WorkCompleted, WorkDomainEvent, WorkInProgressReset, WorkStarted}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.util.{Failure, Success}

/**
  * The work manager actor (hosted by back-end nodes) keeps track of all available workers, and all scheduled and ongoing work items.
  * cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala
  */
object WorkManager {

  val ManagerServiceKey = ServiceKey[ConsumerController.Command[WorkerCommand]]("worker-service-key")

  val WorkerServiceKey: ServiceKey[WorkerCommand] =
    ServiceKey[WorkerCommand]("workerService")
  val ResultsTopic = "results"

  final case class Ack(work: Work) extends CborSerializable

  // Responses to requests from workers
  sealed trait WorkerCommand extends CborSerializable

  final case class DoWork(work: Work) extends WorkerCommand with CborSerializable

  sealed trait Command

  final case class SubmitWork(work: Work, replyTo: ActorRef[WorkManager.Ack])
    extends Command with CborSerializable

  private case class RequestNextWrapper(ask: RequestNext[WorkerCommand]) extends Command

  final case class WorkIsDone(work: Work) extends Command

  final case class WorkFailed(work: Work, t: Throwable) extends Command

  private case object TryStartWork extends Command

  private case object ResetWorkInProgress extends Command

  def apply(workTimeout: FiniteDuration, resultTopicActor: ActorRef[Topic.Command[WorkFinished]]): Behavior[Command] =
    Behaviors.setup { ctx =>
      implicit val timeout = Timeout(60.minutes)
      val producerController = ctx.spawn(WorkPullingProducerController[WorkerCommand]("work-manager", ManagerServiceKey, None), "producer-controller")
      val requestNextAdapter = ctx.messageAdapter(RequestNextWrapper)
      producerController ! WorkPullingProducerController.Start(requestNextAdapter)

      var requestNext: Option[RequestNext[WorkerCommand]] = None

      def tryStartWork(workState: WorkState): Effect[WorkDomainEvent, WorkState] = {

        if (workState.hasWork) {
          requestNext match {
            case Some(next) =>
              val work = workState.nextWork
              ctx.ask[MessageWithConfirmation[WorkerCommand], Done](next.askNextTo, done => MessageWithConfirmation(DoWork(work), done)) {
                case Success(Done) =>
                  WorkIsDone(work)
                case Failure(t) =>
                  ctx.log.error("Work failed", t)
                  WorkFailed(work, t)
              }
              requestNext = None
              Effect.persist(WorkStarted(work.workId))
            case _ =>
              Effect.none
          }
        } else {
          Effect.none
        }
      }

      EventSourcedBehavior[Command, WorkDomainEvent, WorkState](
        persistenceId = PersistenceId.ofUniqueId("master"),
        emptyState = WorkState.empty,
        commandHandler = (workState, command) => {
          command match {
            case RequestNextWrapper(rn) =>
              ctx.log.info("Work request: {}")
              if (requestNext.isDefined) {
                throw new IllegalStateException(s"Request next when there is already demand ${rn}, ${requestNext}")
              }
              requestNext = Some(rn)
              tryStartWork(workState)
            case TryStartWork =>
              tryStartWork(workState)
            case ResetWorkInProgress =>
              Effect.persist(WorkInProgressReset)
            case WorkIsDone(work) =>
              Effect.persist[WorkDomainEvent, WorkState](WorkCompleted(work.workId)).thenRun { newState =>
                ctx.log.info("Work is done {}. New state {}", work.workId, newState)
              }

            case WorkFailed(id, reason) =>
              ctx.log.info("Work failed {} {}", id, reason)
              tryStartWork(workState)
            case submitWork: SubmitWork =>
              // idempotent
              if (workState.isAccepted(submitWork.work.workId)) {
                submitWork.replyTo ! WorkManager.Ack(submitWork.work)
                Effect.none
              } else {
                ctx.log.info("Accepted work: {}", submitWork.work.workId)
                Effect.persist(WorkAccepted(submitWork.work)).thenRun { _ =>
                  // Ack back to original sender
                  submitWork.replyTo ! WorkManager.Ack(submitWork.work)
                  ctx.self ! TryStartWork
                }
              }
          }
        },
        eventHandler = (workState, event) => workState.updated(event)
      ).receiveSignal {
        case (state, RecoveryCompleted) =>
          // Any in progress work from the previous incarnation is retried
          ctx.self ! ResetWorkInProgress
      }
    }

}
