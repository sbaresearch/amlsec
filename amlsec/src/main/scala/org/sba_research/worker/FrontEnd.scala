package org.sba_research.worker

import java.util.UUID

import akka.actor.typed.pubsub.Topic
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, TimerScheduler}
import akka.actor.typed.{Behavior, _}
import akka.util.Timeout
import org.apache.jena.shacl.Shapes
import org.sba_research.Config
import org.sba_research.utils.{PerformanceMeasurement, ReasonerValidationResult}
import org.sba_research.worker.FrontEnd.WorkFinished
import org.sba_research.worker.WorkManager.SubmitWork

import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Front-end actor that sends risk identification workload to the work manager.
  * cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala
  */
object FrontEnd {

  sealed trait Command

  case object StartRiskIdentification extends Command

  case class Failed(work: Work) extends Command

  case class Retry(work: Work) extends Command

  case class WorkAccepted(work: Work) extends Command

  case class WorkFinished(work: Work, result: WorkResult) extends Command with CborSerializable

  sealed trait WorkStatus

  case object ToSubmit extends WorkStatus

  case object Accepted extends WorkStatus

  case object Completed extends WorkStatus

  private def nextId(): String = UUID.randomUUID().toString

  private def nextWorkId(): String = nextId()

  private def nextJobId(): String = nextId()

  def apply(workManager: ActorRef[SubmitWork], resultTopicActor: ActorRef[Topic.Command[WorkFinished]]): Behavior[Command] = Behaviors.setup { ctx =>
    Behaviors.setup { ctx =>
      Behaviors.withTimers { timers =>
        new FrontEnd(workManager, ctx, timers, resultTopicActor).start()
      }
    }
  }

}

class FrontEnd private(workManager: ActorRef[SubmitWork],
                       ctx: ActorContext[FrontEnd.Command],
                       timers: TimerScheduler[FrontEnd.Command],
                       resultTopicActor: ActorRef[Topic.Command[WorkFinished]]) {

  import FrontEnd._

  val config = Config()

  def start(): Behavior[Command] = {
    ctx.log.info("Subscribing to result topic.")
    resultTopicActor ! Topic.Subscribe(ctx.self)
    ctx.self ! StartRiskIdentification

    //timers.startSingleTimer("start", StartRiskIdentification, 60.seconds)

    Behaviors.receiveMessage {
      case StartRiskIdentification =>
        ctx.log.info("Starting risk identification...")
        busy(Map(WorkCreateRemoteDataset(nextJobId(), nextWorkId()) -> ToSubmit), Map.empty[Work, Option[WorkResult]])
      case _ => Behaviors.unhandled
    }
  }

  def idle(): Behavior[Command] = {
    Behaviors.receiveMessage {
      case StartRiskIdentification => start()
      case _ => Behaviors.unhandled
    }
  }

  def busy(workItems: Map[Work, WorkStatus], workResults: Map[Work, Option[WorkResult]] = Map.empty[Work, Option[WorkResult]]): Behavior[Command] = {
    ctx.log.info("Current work items {}", workItems)
    ctx.log.info("Current work results {}", workResults)

    def sendMultipleWorkItems(work: Work, wi: Map[Work, WorkStatus], wr: Map[Work, Option[WorkResult]] = Map.empty[Work, Option[WorkResult]], workResult: Option[WorkResult] = None): Behavior[Command] = {
      /* If work item exists in work status map, update its status to completed. */
      val updatedWorkItems = if (wi.get(work).isDefined) wi + (work -> Completed) else wi
      /* If work item exists in work result map, update its result. */
      val updatedWorkResults = wr + (work -> workResult)
      /* Process updated work items. */
      busy(updatedWorkItems, updatedWorkResults)
    }

    def sendWorkItem(work: Work): Unit = {
      implicit val timeout: Timeout = Timeout(30.seconds)
      ctx.ask[SubmitWork, WorkManager.Ack](workManager, replyTo => SubmitWork(work, replyTo)) {
        case Success(ack) => WorkAccepted(ack.work)
        case Failure(_) => Failed(work)
      }
    }

    def getEngineeringDataValidationWorkItems(jobId: String): Map[Work, WorkStatus] = {
      import scala.jdk.CollectionConverters._
      val shapes = Shapes.parse(config.engValFilePath)
      shapes.getRootShapes.asScala.map { shape => WorkValidateEngineeringData(jobId, nextWorkId(), shape.getShapeNode.getURI) -> ToSubmit }.toMap
    }

    def getSecurityValidationWorkItems(jobId: String): Map[Work, WorkStatus] = {
      import scala.jdk.CollectionConverters._
      val shapes = Shapes.parse(config.secValFilePath)
      shapes.getRootShapes.asScala.map { shape => WorkValidateWithSecurityRule(jobId, nextWorkId(), shape.getShapeNode.getURI) -> ToSubmit }.toMap
    }

    def validationAndRiskIdentificationResultProcessing(work: Work, result: WorkResult): Behavior[Command] = {
      val allJobsFinished = workItems.filter(_._2 != Completed).forall(_._1 == work)
      /* We are not yet done, update work status and stay in busy to wait for completion of remaining work items. */
      if (!allJobsFinished)
        sendMultipleWorkItems(work, workItems, workResults, Some(result))
      else {
        /* Let's check all validation results. */
        val updatedWorkResults = workResults + (work -> Some(result))
        val firstValidationError = updatedWorkResults.find {
          case (work, Some(result: ValidationOfEngineeringDataSuccessful)) => !result.validationReport.conforms
          case (work, Some(result: ValidationWithSecurityRuleSuccessful)) => false
          case _ => false
        }
        firstValidationError match {
          case Some(WorkValidateEngineeringData(_, _, shapeUri) -> Some(ValidationOfEngineeringDataSuccessful(r))) =>
            ctx.log.info("Changing behavior to idle (i.e., stopping risk identification) since engineering data representation does not conform shape '{}'. Results: {}.", shapeUri, r.validationResults)
            idle()
          case _ =>
            ctx.log.info("Validation of engineering data was successful (all tests passed).")
            ctx.log.info("All results: {}", updatedWorkResults)
            val securityValidationReports = updatedWorkResults.flatMap {
              case (_: WorkValidateWithSecurityRule, result: Option[_]) =>
                result.map { case wr: ValidationWithSecurityRuleSuccessful => wr.validationReport }
              case _ => None
            }.toList
            if (securityValidationReports.nonEmpty) {
              ctx.log.info("Starting to submit work to instantiate vulnerabilities based on reports {}", securityValidationReports)
              busy(Map(WorkInstantiateVulnerabilities(work.jobId, nextWorkId(), securityValidationReports) -> ToSubmit))
            }
            else {
              ctx.log.info("No security reports received, changing behavior to idle. ")
              idle() // No security validation report(s), changing to idle
            }
        }
      }
    }

    PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] ${workItems.size} START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
    /* Submit work items one by one (after Ack is received, the next work item will be submitted). */
    workItems.find(_._2 == ToSubmit).foreach { workToSubmit =>
      ctx.log.info("Submitting work item {}.", workToSubmit._1)
      sendWorkItem(workToSubmit._1)
    }

    Behaviors.receiveMessage {
      case Failed(work) =>
        ctx.log.info("Work [jobId={}, workId={}] not accepted, retry after a while", work.jobId, work.workId)
        timers.startSingleTimer("retry", Retry(work), 3.seconds)
        Behaviors.same
      case WorkAccepted(work) =>
        ctx.log.info("Got ack for [jobId={}, workId={}]", work.jobId, work.workId)
        busy(workItems + (work -> Accepted), workResults)
      case Retry(work) =>
        ctx.log.info("Retrying work [jobId={}, workId={}]", work.jobId, work.workId)
        sendWorkItem(work)
        Behaviors.same
      case WorkFinished(work, result) =>
        ctx.log.info("Consumed result: {}", result)
        result match {
          case FusekiDatasetCreationSuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Create Remote Dataset END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (Fuseki dataset creation was successful).")
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Push Model to Remote Dataset START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            busy(Map(WorkPushModelToRemoteDataset(work.jobId, nextWorkId(), config.amlConfig.filePath) -> ToSubmit))
          case FusekiPushModelToRemoteDatasetSuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Push Model to Remote Dataset END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (Fuseki push model to remote dataset was successful).")
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Augment Model START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            busy(Map(WorkAugmentModel(work.jobId, nextWorkId()) -> ToSubmit))
          case ModelAugmentationSuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Augment Model END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (model augmentation was successful).")
            sendMultipleWorkItems(
              work,
              getEngineeringDataValidationWorkItems(work.jobId) ++
                getSecurityValidationWorkItems(work.jobId) ++
                Map(WorkCveCheck(work.jobId, nextWorkId()) -> ToSubmit)
            )
          case ValidationOfEngineeringDataSuccessful(report) =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Validation of engineering data END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (validation of engineering data was successful) '{}'.", report)
            val reasonerValidationResult = report.validationResults.exists(r => r match {
              case res: ReasonerValidationResult => res.isError
              case _ => false
            })
            if (reasonerValidationResult) {
              ctx.log.info("Changing behavior to idle (i.e., stopping risk identification) since engineering data representation is not valid. Results: {}.", report.validationResults.toString)
              idle()
            } else
              validationAndRiskIdentificationResultProcessing(work, result)
          case ValidationWithSecurityRuleSuccessful(report) =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Validation with security rule END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (validation with security rule was successful) '{}'.", report)
            validationAndRiskIdentificationResultProcessing(work, result)
          case CveCheckSuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] CVE Check END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (CVE check was successful).")
            validationAndRiskIdentificationResultProcessing(work, result)
          case InstantiationOfVulnerabilitiesSuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Instantiation of vulnerabilities END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (vulnerability instantiation was successful).")
            busy(Map(WorkGenerateAttackGraph(work.jobId, nextWorkId()) -> ToSubmit))
          case GenerationOfAttackGraphSuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Attack graph generation END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (generation of attack graph was successful).")
            busy(Map(WorkExecuteCaseStudy(work.jobId, nextWorkId()) -> ToSubmit))
          case ExecutionOfCaseStudySuccessful =>
            PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Execution of case study END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Received work result (execution of case study was successful).")
            idle()
          case _ => Behaviors.unhandled
        }
      case StartRiskIdentification => Behaviors.unhandled
    }
  }

}
