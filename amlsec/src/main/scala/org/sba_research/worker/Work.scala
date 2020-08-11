package org.sba_research.worker

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind.annotation.{JsonDeserialize, JsonSerialize}
import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer, JsonNode, JsonSerializer, ObjectMapper, SerializerProvider}
import com.typesafe.scalalogging.Logger
import org.sba_research.utils.{ReasonerValidationResult, ShaclValidationResult, ValidationReport}

// cf. https://github.com/akka/akka-samples/tree/2.6/akka-sample-distributed-workers-scala

@JsonSerialize(using = classOf[WorkSerializer])
@JsonDeserialize(using = classOf[WorkDeserializer])
trait Work extends CborSerializable {
  def jobId: String

  def workId: String
}

case class WorkCreateRemoteDataset(jobId: String, workId: String) extends Work

case class WorkPushModelToRemoteDataset(jobId: String, workId: String, amlFilePath: String) extends Work

case class WorkAugmentModel(jobId: String, workId: String) extends Work

case class WorkValidateEngineeringData(jobId: String, workId: String, shapeUri: String) extends Work

case class WorkValidateWithSecurityRule(jobId: String, workId: String, shapeUri: String) extends Work

case class WorkCveCheck(jobId: String, workId: String) extends Work

case class WorkInstantiateVulnerabilities(jobId: String, workId: String, reports: List[ValidationReport]) extends Work

case class WorkGenerateAttackGraph(jobId: String, workId: String) extends Work

case class WorkExecuteCaseStudy(jobId: String, workId: String) extends Work

class WorkSerializer extends JsonSerializer[Work] {
  val logger = Logger(getClass)

  override def serialize(
                          w: Work,
                          json: JsonGenerator,
                          provider: SerializerProvider
                        ): Unit = {
    json.writeStartObject()
    json.writeFieldName("jobId")
    json.writeString(w.jobId)
    json.writeFieldName("workId")
    json.writeString(w.workId)
    w match {
      case WorkCreateRemoteDataset(_, _) =>
        json.writeFieldName("work_type")
        json.writeString("create_remote_dataset")
      case WorkPushModelToRemoteDataset(_, _, amlFileContent) =>
        json.writeFieldName("work_type")
        json.writeString("push_model_to_remote_dataset")
        json.writeFieldName("aml_file_content")
        json.writeString(amlFileContent)
      case WorkAugmentModel(_, _) =>
        json.writeFieldName("work_type")
        json.writeString("augment_model")
      case WorkValidateEngineeringData(_, _, shapeUri) =>
        json.writeFieldName("work_type")
        json.writeString("validate_engineering_data")
        json.writeFieldName("shape_uri")
        json.writeString(shapeUri)
      case WorkValidateWithSecurityRule(_, _, shapeUri) =>
        json.writeFieldName("work_type")
        json.writeString("validate_with_security_rule")
        json.writeFieldName("shape_uri")
        json.writeString(shapeUri)
      case WorkCveCheck(_, _) =>
        json.writeFieldName("work_type")
        json.writeString("cve_check")
      case WorkInstantiateVulnerabilities(_, _, reports) =>
        json.writeFieldName("work_type")
        json.writeString("instantiate_vulnerabilities")
        json.writeFieldName("reports")
        json.writeStartArray()
        reports.foreach { report =>
          json.writeStartObject()
          json.writeFieldName("conforms")
          json.writeBoolean(report.conforms)
          json.writeFieldName("validation_results")
          json.writeStartArray()
          report.validationResults.foreach { result =>
            json.writeStartObject()
            result match {
              case s: ShaclValidationResult =>
                json.writeFieldName("focus_node")
                json.writeString(s.focusNode)
                json.writeFieldName("result_message")
                json.writeString(s.resultMessage)
                s.resultPath.foreach { rp =>
                  json.writeFieldName("result_path")
                  json.writeString(rp)
                }
                json.writeFieldName("source_shape")
                json.writeString(s.sourceShape)
                s.value.foreach { v =>
                  json.writeFieldName("value")
                  json.writeString(v)
                }
              case r: ReasonerValidationResult =>
                json.writeFieldName("type")
                json.writeString(r.`type`)
                json.writeFieldName("is_error")
                json.writeBoolean(r.isError)
                json.writeFieldName("description")
                json.writeString(r.description)
            }
            json.writeEndObject()
          }
          json.writeEndArray()
          json.writeEndObject()
        }
        json.writeEndArray()
      case WorkGenerateAttackGraph(_, _) =>
        json.writeFieldName("work_type")
        json.writeString("generate_attack_graph")
      case WorkExecuteCaseStudy(_, _) =>
        json.writeFieldName("work_type")
        json.writeString("execute_case_study")
      case _ =>
        logger.debug(s"Could not match work item when serializing [$w].")
    }
    json.writeEndObject()
  }
}

class WorkDeserializer extends JsonDeserializer[Work] {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext): Work = {
    import com.fasterxml.jackson.databind.JsonNode
    val node: JsonNode = p.getCodec.readTree(p)
    val jobIdField = node.get("jobId")
    val jobId = jobIdField.asText()
    val workIdField = node.get("workId")
    val workId = workIdField.asText()
    val workTypeField = Option(node.get("work_type")) // may be null
    val workType = workTypeField map (_.asText)
    val workItem = workType map {
      case "create_remote_dataset" => WorkCreateRemoteDataset(jobId, workId)
      case "push_model_to_remote_dataset" =>
        val amlFileContentField = node.get("aml_file_content")
        val amlFileContent = amlFileContentField.asText()
        WorkPushModelToRemoteDataset(jobId, workId, amlFileContent)
      case "augment_model" => WorkAugmentModel(jobId, workId)
      case "validate_engineering_data" =>
        val shapeUriField = node.get("shape_uri")
        val shapeUri = shapeUriField.asText()
        WorkValidateEngineeringData(jobId, workId, shapeUri)
      case "validate_with_security_rule" =>
        val shapeUriField = node.get("shape_uri")
        val shapeUri = shapeUriField.asText()
        WorkValidateWithSecurityRule(jobId, workId, shapeUri)
      case "cve_check" => WorkCveCheck(jobId, workId)
      case "instantiate_vulnerabilities" =>
        val reportsField = node.get("reports")
        import scala.jdk.CollectionConverters._
        val reports = reportsField.asScala.map { report =>
          val conformsField = report.get("conforms")
          val conforms = conformsField.asBoolean()
          val validationResultsField = report.get("validation_results")
          val validationResults = validationResultsField.asScala.map { result =>
            if (result.has("focus_node")) { // Check if we need to deserialize a SHACL Result
              val focusNodeField = result.get("focus_node")
              val focusNode = focusNodeField.asText()
              val resultMessageField = result.get("result_message")
              val resultMessage = resultMessageField.asText()
              val resultPathField = Option(result.get("result_path"))
              val resultPath = resultPathField.map(_.asText)
              val sourceShapeField = result.get("source_shape")
              val sourceShape = sourceShapeField.asText()
              val valueField = Option(result.get("value"))
              val value = valueField.map(_.asText)
              ShaclValidationResult(focusNode, resultMessage, resultPath, sourceShape, value)
            } else if (result.has("type")) { // Reasoner result
              val typeField = result.get("type")
              val `type` = typeField.asText()
              val isErrorField = result.get("is_error")
              val isError = isErrorField.asBoolean()
              val descriptionField = result.get("description")
              val description = descriptionField.asText
              ReasonerValidationResult(`type` = `type`, isError, description)
            }
            else throw new IllegalStateException("Unknown validation result instance.")
          }.toList
          ValidationReport(conforms, validationResults)
        }.toList
        WorkInstantiateVulnerabilities(jobId, workId, reports)
      case "generate_attack_graph" => WorkGenerateAttackGraph(jobId, workId)
      case "execute_case_study" => WorkExecuteCaseStudy(jobId, workId)
      case _ => throw new IllegalArgumentException(
        s"[$workType] is not a valid value for work type."
      )
    }
    workItem.getOrElse(
      throw new IllegalArgumentException(
        s"Could not deserialize work item [${node.toPrettyString}]."
      )
    )
  }
}

@JsonSerialize(using = classOf[WorkResultSerializer])
@JsonDeserialize(using = classOf[WorkResultDeserializer])
sealed trait WorkResult extends CborSerializable

case object FusekiDatasetCreationSuccessful extends WorkResult

case object FusekiPushModelToRemoteDatasetSuccessful extends WorkResult

case object ModelAugmentationSuccessful extends WorkResult

case class ValidationOfEngineeringDataSuccessful(validationReport: ValidationReport) extends WorkResult

case class ValidationWithSecurityRuleSuccessful(validationReport: ValidationReport) extends WorkResult

case object CveCheckSuccessful extends WorkResult

case object InstantiationOfVulnerabilitiesSuccessful extends WorkResult

case object GenerationOfAttackGraphSuccessful extends WorkResult

case object ExecutionOfCaseStudySuccessful extends WorkResult

class WorkResultSerializer extends JsonSerializer[WorkResult] {
  val logger = Logger(getClass)

  override def serialize(
                          wr: WorkResult,
                          json: JsonGenerator,
                          provider: SerializerProvider
                        ): Unit = {
    json.writeStartObject()
    wr match {
      case FusekiDatasetCreationSuccessful =>
        json.writeFieldName("type")
        json.writeString("fuseki_dataset_creation")
      case FusekiPushModelToRemoteDatasetSuccessful =>
        json.writeFieldName("type")
        json.writeString("fuseki_push_model_to_remote_dataset")
      case ModelAugmentationSuccessful =>
        json.writeFieldName("type")
        json.writeString("augment_model")
      case ValidationOfEngineeringDataSuccessful(report) =>
        json.writeFieldName("type")
        json.writeString("validation_of_engineering_data")
        json.writeFieldName("validation_report")
        json.writeObject(report)
      case ValidationWithSecurityRuleSuccessful(report) =>
        json.writeFieldName("type")
        json.writeString("validation_with_security_rule")
        json.writeFieldName("validation_report")
        json.writeObject(report)
      case CveCheckSuccessful =>
        json.writeFieldName("type")
        json.writeString("cve_check")
      case InstantiationOfVulnerabilitiesSuccessful =>
        json.writeFieldName("type")
        json.writeString("instantiate_vulnerabilities")
      case GenerationOfAttackGraphSuccessful =>
        json.writeFieldName("type")
        json.writeString("generate_attack_graph")
      case ExecutionOfCaseStudySuccessful =>
        json.writeFieldName("type")
        json.writeString("execute_case_study")
      case _ =>
        logger.debug(s"Could not match work result when serializing [$wr].")
    }
    json.writeEndObject()
  }
}

class WorkResultDeserializer extends JsonDeserializer[WorkResult] {
  val logger = Logger(getClass)

  private def readValidationReportObject(p: JsonParser, node: JsonNode): ValidationReport = {
    val validationReportFieldType = node.get("validation_report")
    val validationReport = validationReportFieldType.traverse(p.getCodec).readValueAs(classOf[ValidationReport])
    validationReport
  }

  override def deserialize(p: JsonParser, ctxt: DeserializationContext): WorkResult = {
    import com.fasterxml.jackson.databind.JsonNode
    val node: JsonNode = p.getCodec.readTree(p)
    val workResultTypeField = Option(node.get("type")) // may be null
    val workResultType = workResultTypeField map (_.asText(""))
    val workResult = workResultType map {
      case "fuseki_dataset_creation" => FusekiDatasetCreationSuccessful
      case "fuseki_push_model_to_remote_dataset" => FusekiPushModelToRemoteDatasetSuccessful
      case "augment_model" => ModelAugmentationSuccessful
      case "validation_of_engineering_data" => ValidationOfEngineeringDataSuccessful(readValidationReportObject(p, node))
      case "validation_with_security_rule" => ValidationWithSecurityRuleSuccessful(readValidationReportObject(p, node))
      case "cve_check" => CveCheckSuccessful
      case "instantiate_vulnerabilities" => InstantiationOfVulnerabilitiesSuccessful
      case "generate_attack_graph" => GenerationOfAttackGraphSuccessful
      case "execute_case_study" => ExecutionOfCaseStudySuccessful
      case _ => throw new IllegalArgumentException(
        s"[$workResultType] is not a valid value for work type."
      )
    }
    workResult.getOrElse(
      throw new IllegalArgumentException(
        s"Could not deserialize work result [${node.toPrettyString}]."
      )
    )
  }
}