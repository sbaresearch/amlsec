package org.sba_research.worker

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior, DispatcherSelector}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import org.apache.jena.ontology.{OntModel, OntModelSpec}
import org.apache.jena.rdf.model.{InfModel, Model, ModelFactory}
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionFactory}
import org.apache.jena.reasoner.rulesys.OWLMicroReasonerFactory
import org.apache.jena.reasoner.{InfGraph, ValidityReport}
import org.apache.jena.vocabulary.OWL2
import org.sba_research.casestudy.CaseStudy
import org.sba_research.model.{AmlOntExtension, OntModels}
import org.sba_research.utils.{OntModelUtils, PerformanceMeasurement, ValidationReport, Validator}
import org.sba_research.worker.WorkExecutor.ExecuteWork
import org.sba_research.worker.Worker.{Message, ValidationWorkResult, WorkComplete, WorkFailed}
import org.sba_research._
import org.sba_research.ag.AttackGraph
import org.sba_research.vuln.{CveChecker, VulnerabilityModeling}

import scala.util.{Failure, Success, Using}

/**
  * Work executor is the actor actually performing the work.
  * cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala
  */
object WorkExecutor {

  sealed trait ExecuteWork {
    def work: Work

    def replyTo: ActorRef[Message]
  }

  case class CreateRemoteDataset(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class RemoteDatasetCreationSuccessful(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class RemoteDatasetCreationFailed(work: Work, replyTo: ActorRef[Message], res: HttpResponse) extends ExecuteWork

  case class FailedToSendHttpRequestToFuseki(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class PushModelToRemoteDataset(work: Work, replyTo: ActorRef[Message], amlFilePath: String) extends ExecuteWork

  case class PushModelToRemoteDatasetSuccessful(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class PushModelToRemoteDatasetFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class ConvertAmlToOntologySuccessful(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class ConvertAmlToOntologyFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class AugmentModel(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class ModelAugmentationSuccessful(work: Work, replyTo: ActorRef[Message], model: Model) extends ExecuteWork

  case class ModelAugmentationFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class ValidateEngineeringData(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class ValidateEngineeringDataSuccessful(work: Work, replyTo: ActorRef[Message], result: ValidationWorkResult, model: Option[Model]) extends ExecuteWork

  case class ValidateEngineeringDataFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class ValidateWithSecurityRule(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class ValidationWithSecurityRuleSuccessful(work: Work, replyTo: ActorRef[Message], result: ValidationWorkResult, model: Model) extends ExecuteWork

  case class ValidationWithSecurityRuleFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class PerformCveCheck(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class CveCheckSuccessful(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class CveCheckFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class InstantiateVulnerabilities(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class VulnerabilityInstantiationSuccessful(work: Work, replyTo: ActorRef[Message], model: Model) extends ExecuteWork

  case class VulnerabilityInstantiationFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class GenerateAttackGraph(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class AttackGraphGenerationSuccessful(work: Work, replyTo: ActorRef[Message], model: Model) extends ExecuteWork

  case class AttackGraphGenerationFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  case class ExecuteCaseStudy(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class ExecutionOfCaseStudySuccessful(work: Work, replyTo: ActorRef[Message]) extends ExecuteWork

  case class ExecutionOfCaseStudyFailed(work: Work, replyTo: ActorRef[Message], e: Throwable) extends ExecuteWork

  def apply(): Behavior[ExecuteWork] = Behaviors.setup { ctx =>
    val config = Config()
    OntModelUtils.setupLocationMappings(config)
    new WorkExecutor(config, ctx).main()
  }
}

class WorkExecutor private(config: Config, ctx: ActorContext[ExecuteWork]) {

  import WorkExecutor._

  def main(cachedModels: Map[String, Model] = Map.empty): Behavior[ExecuteWork] =
    Behaviors.receiveMessage {
      case crd@CreateRemoteDataset(work, replyTo) =>
        ctx.log.info("Doing work (creating remote dataset) {}", crd)
        createDatasetOnFusekiSrv(config, work, ctx, replyTo)
        Behaviors.same
      case RemoteDatasetCreationSuccessful(work, replyTo) =>
        ctx.log.info("Successfully created dataset '{}' on Fuseki server ({}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        Behaviors.same
      case RemoteDatasetCreationFailed(work, replyTo, res) =>
        ctx.log.error("Could not create dataset '{}' on Fuseki server ({}), received response ({}).", work.jobId, config.fusekiConfig.uri, res)
        replyTo ! WorkFailed("Could not create remote dataset.", null)
        Behaviors.same
      case FailedToSendHttpRequestToFuseki(work, replyTo) =>
        ctx.log.error("Failed to send HTTP request to Fuseki server ({}) to create dataset '{}'.", config.fusekiConfig.uri, work.jobId)
        replyTo ! WorkFailed("Could not sent HTTP request to Fuseki to create remote dataset.", new IllegalStateException())
        Behaviors.same
      case pmtrd@PushModelToRemoteDataset(work, replyTo, amlFilePath) =>
        ctx.log.info("Doing work (pushing model to remote dataset) {}", pmtrd)
        convertAmlFileToOntology(config, work, ctx, replyTo, amlFilePath)
        Behaviors.same
      case PushModelToRemoteDatasetSuccessful(work, replyTo) =>
        ctx.log.info("Successfully pushed model to dataset '{}' on Fuseki server ({}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        Behaviors.same
      case PushModelToRemoteDatasetFailed(work, replyTo, e) =>
        ctx.log.error("Could not push model to dataset '{}' on Fuseki server ({}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not push model to remote dataset.", e)
        Behaviors.same
      case ConvertAmlToOntologySuccessful(work, replyTo) =>
        ctx.log.info("Successfully converted AML file to ontology (jobId '{}').", work.jobId, config.fusekiConfig.uri)
        pushModelToFusekiSrv(config, work, ctx, replyTo)
        Behaviors.same
      case ConvertAmlToOntologyFailed(work, replyTo, e) =>
        ctx.log.error("Could not convert AML file to onotlogy (jobId '{}'), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not convert AML file to ontology.", e)
        Behaviors.same
      case aoa@AugmentModel(work, replyTo) =>
        ctx.log.info("Doing work (augmenting model) {}", aoa)
        augmentModel(config, work, ctx, replyTo)
        Behaviors.same
      case ModelAugmentationSuccessful(work, replyTo, model) =>
        ctx.log.info("Successfully augmented model (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        // main(cachedModels + (work.jobId -> model)) <--- Strangely, caching this model will sometimes produce wrong validation results
        Behaviors.same
      case ModelAugmentationFailed(work, replyTo, e) =>
        ctx.log.error("Could not augment model (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not augment model.", e)
        Behaviors.same
      case ved@ValidateEngineeringData(work, replyTo) =>
        ctx.log.info("Doing work (validate engineering data) {}", ved)
        validateEngineeringData(config, work, ctx, replyTo, cachedModels)
        Behaviors.same
      case ValidateEngineeringDataSuccessful(work, replyTo, result, model) =>
        ctx.log.info("Successfully validated engineering data (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work, Some(result))
        model match {
          case Some(m) => main(cachedModels + (work.jobId -> m))
          case None => Behaviors.same
        }
      case ValidateEngineeringDataFailed(work, replyTo, e) =>
        ctx.log.error("Could not validate engineering data (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not validate engineering data.", e)
        Behaviors.same
      case vwsr@ValidateWithSecurityRule(work, replyTo) =>
        ctx.log.info("Doing work (validate with security rule) {}", vwsr)
        validateWithSecurityRule(config, work, ctx, replyTo, cachedModels)
        Behaviors.same
      case ValidationWithSecurityRuleSuccessful(work, replyTo, result, model) =>
        ctx.log.info("Successfully validated with security rule (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work, Some(result))
        main(cachedModels + (work.jobId -> model))
      case ValidationWithSecurityRuleFailed(work, replyTo, e) =>
        ctx.log.error("Could not validate with security rule (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not validate with security rule.", e)
        Behaviors.same
      case cve@PerformCveCheck(work, replyTo) =>
        ctx.log.info("Doing work (performing CVE check) {}", cve)
        performCveCheck(config, work, ctx, replyTo, cachedModels)
        Behaviors.same
      case CveCheckSuccessful(work, replyTo) =>
        ctx.log.info("Successfully performed CVE check (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        Behaviors.same
      case CveCheckFailed(work, replyTo, e) =>
        ctx.log.error("Could not perform CVE check (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not perform CVE check.", e)
        Behaviors.same
      case vulns@InstantiateVulnerabilities(work, replyTo) =>
        ctx.log.info("Doing work (instantiate vulnerabilities) {}", vulns)
        instantiateVulnerabilities(config, work, ctx, replyTo)
        Behaviors.same
      case VulnerabilityInstantiationSuccessful(work, replyTo, model) =>
        ctx.log.info("Successfully instantiated vulnerabilities (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        main(cachedModels + (work.jobId -> model))
      case VulnerabilityInstantiationFailed(work, replyTo, e) =>
        ctx.log.error("Could not instantiate vulnerabilities (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not instantiate vulnerabilities.", e)
        Behaviors.same
      case ag@GenerateAttackGraph(work, replyTo) =>
        ctx.log.info("Doing work (generate attack graph) {}", ag)
        generateAttackGraph(config, work, ctx, replyTo)
        Behaviors.same
      case AttackGraphGenerationSuccessful(work, replyTo, model) =>
        ctx.log.info("Successfully generated attack graph (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        main(cachedModels + (work.jobId -> model))
      case AttackGraphGenerationFailed(work, replyTo, e) =>
        ctx.log.error("Could not generate attack graph (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not instantiate vulnerabilities.", e)
        Behaviors.same
      case ec@ExecuteCaseStudy(work, replyTo) =>
        ctx.log.info("Doing work (execute case study) {}", ec)
        executeCaseStudy(config, work, ctx, replyTo)
        Behaviors.same
      case ExecutionOfCaseStudySuccessful(work, replyTo) =>
        ctx.log.info("Successfully executed case study (dataset '{}' on Fuseki server {}).", work.jobId, config.fusekiConfig.uri)
        replyTo ! WorkComplete(work)
        Behaviors.same
      case ExecutionOfCaseStudyFailed(work, replyTo, e) =>
        ctx.log.error("Could not execute case study (dataset '{}' on Fuseki server {}), exception ({}).", work.jobId, config.fusekiConfig.uri, e)
        replyTo ! WorkFailed("Could not execute case study.", e)
        Behaviors.same
    }

  private def createDatasetOnFusekiSrv(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message]): Unit = {
    implicit val system = ctx.system
    implicit val executionContext = system.dispatchers.lookup(DispatcherSelector.default())
    val reqCreateDataset = Http().singleRequest(
      HttpRequest(
        method = HttpMethods.POST,
        uri = s"${config.fusekiConfig.uri}/$$/datasets",
        entity = HttpEntity(ContentTypes.`application/x-www-form-urlencoded`, s"dbType=tdb&dbName=${work.jobId}")
      )
    )
    reqCreateDataset.onComplete {
      case Success(res) =>
        if (res.status.isSuccess())
          ctx.self ! RemoteDatasetCreationSuccessful(work, replyTo)
        else ctx.self ! RemoteDatasetCreationFailed(work, replyTo, res)
      case Failure(_) => ctx.self ! FailedToSendHttpRequestToFuseki(work, replyTo)
    }
  }

  private def convertAmlFileToOntology(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message], amlFilePath: String): Unit = {
    PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] AML-to-OWL Transformation START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
    val pb = new ProcessBuilder("java", "-jar", config.amlToOwlProgram, amlFilePath, s"${config.baseDir + work.jobId}.owl")
    pb.inheritIO()
    val process = pb.start()
    import ctx.executionContext

    import scala.jdk.FutureConverters._
    process.onExit().asScala.onComplete {
      case Success(process) =>
        PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] AML-to-OWL Transformation END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        if (process.exitValue() == 0)
          ctx.self ! ConvertAmlToOntologySuccessful(work, replyTo)
        else ctx.self ! ConvertAmlToOntologyFailed(work, replyTo, throw new IllegalStateException("Could not convert AML file to ontology."))
      case Failure(e) => ctx.self ! ConvertAmlToOntologyFailed(work, replyTo, e)
    }
  }

  private def pushModelToFusekiSrv(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message]): Unit = {
    ctx.log.info("Pushing model to dataset '{}' on Fuseki server ({}).", work.jobId, config.fusekiConfig.uri)
    val ontModels = OntModels(config)
    val amlOntModel = OntModelUtils.createModel(s"${config.baseDir + work.jobId}.owl", "aml")
    Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
      val mergedModel = amlOntModel.union(ontModels.sec).union(ontModels.icssec).union(ontModels.ag)
      rdfConn.put(mergedModel)
    } match {
      case Success(_) => ctx.self ! PushModelToRemoteDatasetSuccessful(work, replyTo)
      case Failure(e) => ctx.self ! PushModelToRemoteDatasetFailed(work, replyTo, e)
    }
  }

  private def getOntModelFromRemoteDataset(rdfConn: RDFConnection): OntModel =
    ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM, rdfConn.fetchDataset().getDefaultModel)

  private def pushUpdatedModelToRemoteDataset(rdfConn: RDFConnection, model: Model): Unit = {
    rdfConn.delete()
    rdfConn.put(model)
  }

  private def augmentModel(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message]): Unit = {

    /**
      * Used to obtain an inference model that is supported by the selected reasoner.
      *
      * @param infModel    the inference model to modify
      * @param reasonerUri the URI of the reasoner
      * @return an inference model suitable for validation with the selected reasoner
      */
    def getInfModelForReasonerValidityCheck(infModel: InfModel, reasonerUri: String): InfModel = reasonerUri match {
      case OWLMicroReasonerFactory.URI =>
        /* Create a deep copy of the inference model. */
        val copy = ModelFactory.createInfModel(infModel.getGraph.asInstanceOf[InfGraph])
        /* Remove all disjoint axioms, as they are not supported by the OWL Micro reasoner. */
        copy.removeAll(null, OWL2.disjointUnionOf, null)
        copy.removeAll(null, OWL2.disjointWith, null)
        copy
      case _ => infModel
    }

    /**
      * This is a custom validity check that suppresses the following errors if the OWL Micro Reasoner is used:
      * ```
      * Property http://www.w3.org/2000/01/rdf-schema#label has a typed range Datatype[http://www.w3.org/2000/01/rdf-schema#Literal]that is not compatible with "text"@en
      * Property http://www.w3.org/2000/01/rdf-schema#comment has a typed range Datatype[http://www.w3.org/2000/01/rdf-schema#Literal]that is not compatible with "text"@en
      * ```
      *
      * @param validityReport the validity report
      * @param reasonerUri    the reasoner URI
      * @return true, if the model is valid
      */
    def isModelValid(validityReport: ValidityReport, reasonerUri: String): Boolean = reasonerUri match {
      case OWLMicroReasonerFactory.URI =>
        import scala.jdk.CollectionConverters._
        /* Suppress dtRange errors. Needs to be fixed in a future release. */
        !validityReport.getReports.asScala.toList
          .exists(r => r.`type` != "dtRange" && (!r.description.contains("label") || !r.description.contains("comment")))
      case _ => validityReport.isValid
    }

    Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
      val model = getOntModelFromRemoteDataset(rdfConn)
      PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Adding axioms START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      val augmentedModel = AmlOntExtension.addOntAxioms(config, model)
      PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Adding axioms END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      /*
      augmentedModel.foreach { m =>
        OntModelUtils.write(m, "/home/meckhart/amlsec/augmented_model.owl")
      }
       */
      augmentedModel.flatMap { m =>
        ctx.log.info("Obtaining inference model...")
        PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Reasoner Get Inferences START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        val infModel = OntModelUtils.getInfModel(m, config.reasonerUri)
        PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Reasoner Get Inferences END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        ctx.log.info("Testing the validity of data using the reasoner...")
        PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Reasoner Validate Data START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        val validityReport = getInfModelForReasonerValidityCheck(infModel, config.reasonerUri).validate()
        PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Reasoner Validate Data END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        ctx.log.info("Finished validity test with reasoner.")
        if (isModelValid(validityReport, config.reasonerUri)) {
          ctx.log.info("Adding asset physically connected to asset roles...")
          PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Add Asset Physically Connected to Asset Roles START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
          val physicallyConnAugModel = AmlOntExtension.addAssetPhysicallyConnectedToAssetRoles(config, infModel, withInference = false)
          PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Add Asset Physically Connected to Asset Roles END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
          ctx.log.info("Adding asset logically connected to asset roles...")
          PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Add Asset Logically Connected to Asset Roles START"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
          val logicallyConnAugModel = AmlOntExtension.addAssetLogicallyConnectedToAssetRoles(config, infModel, withInference = false)
          PerformanceMeasurement.writeElapsedTimeToFile(Some("[Main] Add Asset Logically Connected to Asset Roles END"), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
          for {
            pm <- physicallyConnAugModel
            lm <- logicallyConnAugModel
          } yield {
            val resultModel = infModel.add(pm).add(lm)
            pushUpdatedModelToRemoteDataset(rdfConn, resultModel)
            resultModel
          }
        } else {
          import scala.jdk.CollectionConverters._
          validityReport.getReports.asScala.toList.foreach(x => println(x.description))
          Some(Validator.processReasonerValidityReports(validityReport.getReports.asScala.toList))
        }
      }
    } match {
      case Success(Some(result)) =>
        result match {
          case model: Model => ctx.self ! ModelAugmentationSuccessful(work, replyTo, model)
          case validationReport: ValidationReport => ctx.self ! ValidateEngineeringDataSuccessful(work, replyTo, ValidationWorkResult(validationReport), None)
          case _ => ctx.self ! ModelAugmentationFailed(work, replyTo, throw new IllegalStateException("Could not perform model augmentation."))
        }
      case Success(None) => ctx.self ! ModelAugmentationFailed(work, replyTo, throw new IllegalStateException("Could not perform model augmentation."))
      case Failure(e) => ctx.self ! ModelAugmentationFailed(work, replyTo, e)
    }
  }

  private def validateEngineeringData(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message], cachedModels: Map[String, Model]): Unit =
    work match {
      case WorkValidateEngineeringData(jobId, workId, shapeUri) =>
        val validation = cachedModels.get(jobId) match {
          case Some(m) =>
            ctx.log.info("Starting validation of engineering data with cached model.")
            PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri with cached model START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            val r = validate(config, m, shapeUri, config.engValFilePath, Some(config.outputPathEngValReport))
            PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri with cached model END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Finished validation of engineering data with cached model.")
            Success(r, m)
          case None =>
            Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
              val m = getOntModelFromRemoteDataset(rdfConn)
              ctx.log.info("Starting validation of engineering data.")
              PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
              val r = validate(config, m, shapeUri, config.engValFilePath, Some(config.outputPathEngValReport))
              PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
              ctx.log.info("Finished validation of engineering data.")
              (r, m)
            }
        }
        validation match {
          case Success((Some(report), model)) =>
            ctx.log.info(report.toString)
            ctx.self ! ValidateEngineeringDataSuccessful(work, replyTo, ValidationWorkResult(report), Some(model))
          case Success((None, _)) => ctx.self ! ValidateEngineeringDataFailed(work, replyTo, throw new RuntimeException(s"Could not validate with shapes model using URI $shapeUri."))
          case Failure(e) => ctx.self ! ValidateEngineeringDataFailed(work, replyTo, e)
        }
      case _ => ctx.self ! ValidateEngineeringDataFailed(work, replyTo, throw new IllegalStateException(s"Expected validate engineering data work item, but got ${work.toString}."))
    }


  private def validateWithSecurityRule(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message], cachedModels: Map[String, Model]): Unit =
    work match {
      case WorkValidateWithSecurityRule(jobId, workId, shapeUri) =>
        val validation = cachedModels.get(jobId) match {
          case Some(m) =>
            ctx.log.info("Starting validation with security rule with cached model.")
            PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri with cached model START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            val r = validate(config, m, shapeUri, config.secValFilePath, Some(config.outputPathSecValReport))
            PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri with cached model END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
            ctx.log.info("Finished validation with security rule with cached model.")
            Success(r, m)
          case None =>
            Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
              val m = getOntModelFromRemoteDataset(rdfConn)
              ctx.log.info("Starting validation with security rule.")
              PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
              val r = validate(config, m, shapeUri, config.secValFilePath, Some(config.outputPathSecValReport))
              PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Validation with shape $shapeUri END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
              ctx.log.info("Finished validation with security rule.")
              (r, m)
            }
        }
        validation match {
          case Success((Some(report), model)) =>
            ctx.log.info(report.toString)
            ctx.self ! ValidationWithSecurityRuleSuccessful(work, replyTo, ValidationWorkResult(report), model)
          case Success((None, _)) => ctx.self ! ValidationWithSecurityRuleFailed(work, replyTo, throw new RuntimeException(s"Could not validate with shapes model using URI $shapeUri."))
          case Failure(e) => ctx.self ! ValidationWithSecurityRuleFailed(work, replyTo, e)
        }
      case _ => ctx.self ! ValidationWithSecurityRuleFailed(work, replyTo, throw new IllegalStateException(s"Expected validate with security rule work item, but got ${work.toString}."))
    }

  private def validate(config: Config, model: Model, shapeUri: String, shapesModelPath: String, outputReportPath: Option[String]): Option[ValidationReport] =
    Validator.validate(
      model,
      shapesModelPath = shapesModelPath,
      reportPath = outputReportPath,
      shapeUri
    )

  private def performCveCheck(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message], cachedModels: Map[String, Model]): Unit = {
    Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
      val model = cachedModels.get(work.jobId).getOrElse(getOntModelFromRemoteDataset(rdfConn))
      ctx.log.info("Starting CVE check.")
      PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] CVE check START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      val ontModel = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM, model)
      val vulnModel = VulnerabilityModeling.processCves(config, ontModel, CveChecker.check(ontModel))
      vulnModel map { r => pushUpdatedModelToRemoteDataset(rdfConn, r) }
      PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] CVE check END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      ctx.log.info("Finished CVE check.")
      vulnModel
    } match {
      case Success(Some(_)) => ctx.self ! CveCheckSuccessful(work, replyTo)
      case Success(None) => ctx.self ! CveCheckFailed(work, replyTo, throw new RuntimeException(s"Could not perform CVE check."))
      case Failure(e) => ctx.self ! CveCheckFailed(work, replyTo, e)
    }
  }

  private def instantiateVulnerabilities(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message]): Unit = work match {
    case WorkInstantiateVulnerabilities(jobId, workId, reports) =>
      Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
        val ontModel = getOntModelFromRemoteDataset(rdfConn)
        ctx.log.info("Starting instantiation of vulnerabilities.")
        PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Vulnerability instantiation START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        val vulnModel = VulnerabilityModeling.processReports(config, Some(ontModel), infModel = None, reports)
        PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Vulnerability instantiation END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
        ctx.log.info("Finished instantiation of vulnerabilities.")
        vulnModel.foreach { m => pushUpdatedModelToRemoteDataset(rdfConn, m) }
        vulnModel
      } match {
        case Success(Some(m)) => ctx.self ! VulnerabilityInstantiationSuccessful(work, replyTo, m)
        case Success(None) => ctx.self ! VulnerabilityInstantiationFailed(work, replyTo, throw new RuntimeException(s"Could not perform vulnerability instantiation."))
        case Failure(e) => ctx.self ! VulnerabilityInstantiationFailed(work, replyTo, e)
      }
    case _ => ctx.self ! VulnerabilityInstantiationFailed(work, replyTo, throw new IllegalStateException(s"Expected instantiate vulnerabilities work item, but got ${work.toString}."))
  }

  private def generateAttackGraph(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message]): Unit =
    Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
      val model = getOntModelFromRemoteDataset(rdfConn)
      ctx.log.info("Starting attack graph generation.")
      PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Attack graph generation START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      val agModel = AttackGraph.generate(config, model, withInference = false)
      PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Attack graph generation END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      ctx.log.info("Finished generation of attack graph.")
      agModel.map { m =>
        val resultModel = model.add(m)
        pushUpdatedModelToRemoteDataset(rdfConn, resultModel)
        resultModel
      }
    } match {
      case Success(Some(m)) => ctx.self ! AttackGraphGenerationSuccessful(work, replyTo, m)
      case Success(None) => ctx.self ! AttackGraphGenerationFailed(work, replyTo, throw new RuntimeException(s"Could not generate attack graph."))
      case Failure(e) => ctx.self ! AttackGraphGenerationFailed(work, replyTo, e)
    }

  private def executeCaseStudy(config: Config, work: Work, ctx: ActorContext[WorkExecutor.ExecuteWork], replyTo: ActorRef[Message]): Unit = {
    Using(RDFConnectionFactory.connect(s"${config.fusekiConfig.uri}/${work.jobId}")) { rdfConn =>
      val model = getOntModelFromRemoteDataset(rdfConn)
      ctx.log.info("Starting case study.")
      PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Case study START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      CaseStudy.plotCpag(config, model, withInference = false)
      PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Case study END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
      ctx.log.info("Finished case study.")
    } match {
      case Success(_) => ctx.self ! ExecutionOfCaseStudySuccessful(work, replyTo)
      case Failure(e) => ctx.self ! ExecutionOfCaseStudyFailed(work, replyTo, e)
    }
  }

}