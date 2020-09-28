package org.sba_research.utils

import java.io.{File, FileOutputStream}

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer, JsonSerializer, SerializerProvider}
import com.fasterxml.jackson.databind.annotation.{JsonDeserialize, JsonSerialize}
import com.typesafe.scalalogging.Logger
import org.apache.jena.rdf.model.{InfModel, Model, Resource}
import org.apache.jena.reasoner.ValidityReport
import org.apache.jena.riot.{RDFDataMgr, RDFFormat}
import org.apache.jena.shacl.{ShaclValidator, Shapes}
import org.apache.jena.vocabulary.RDFS
import org.sba_research.model.OntModels
import org.sba_research.worker.CborSerializable
import org.topbraid.jenax.util.{ARQFactory, JenaUtil}
import org.topbraid.shacl.arq.SHACLFunctions
import org.topbraid.shacl.engine.{Shape, ShapesGraph}
import org.topbraid.shacl.model.SHFactory
import org.topbraid.shacl.util.{ModelPrinter, SHACLUtil}
import org.topbraid.shacl.validation.{ValidationEngine, ValidationEngineConfiguration, ValidationEngineFactory, ValidationUtil}
import org.topbraid.shacl.vocabulary.SH

import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

@JsonSerialize(using = classOf[ValidationResultSerializer])
@JsonDeserialize(using = classOf[ValidationResultDeserializer])
sealed trait ValidationResult extends CborSerializable

case class ValidationReport(conforms: Boolean, validationResults: List[ValidationResult])

case class ShaclValidationResult(focusNode: String, resultMessage: String, resultPath: Option[String] = None, sourceShape: String, value: Option[String] = None) extends ValidationResult

case class ReasonerValidationResult(`type`: String, isError: Boolean, description: String) extends ValidationResult

object Validator {

  val logger = Logger(getClass)

  @Deprecated
  def validate(ontModels: OntModels, shapesModelPath: String, reportPath: Option[String], reasonerUri: String): ValidationReport = {
    val amlModel = ontModels.aml.getOrElse(throw new IllegalStateException("Could not obtain AutomationML model. Path to corresponding OWL file may be wrong."))
    val infModel = OntModelUtils.getInfModel(amlModel, reasonerUri)
    validateUsingValidationUtil(infModel = infModel, shapesModelPath = shapesModelPath, reportPath)
  }

  def validate(infModel: InfModel, shapesModelPath: String, reportPath: Option[String]): ValidationReport = {
    validateUsingValidationUtil(infModel = infModel, shapesModelPath = shapesModelPath, reportPath)
  }

  def validate2(infModel: InfModel, shapesModelPath: String, reportPath: Option[String]): ValidationReport = {
    val shapes = Shapes.parse(shapesModelPath)
    val report = ShaclValidator.get().validate(shapes, infModel.getGraph)
    reportPath match {
      case Some(path) => writeReport(path, report.getResource)
      case _ =>
    }
    val validationResults = report.getEntries.asScala.toList.map { entry =>
      ShaclValidationResult(
        focusNode = entry.focusNode().toString,
        resultMessage = entry.message(),
        resultPath = Option(entry.resultPath()).map(_.toString),
        sourceShape = entry.source().toString,
        value = Option(entry.value()).map(_.toString())
      )
    }
    ValidationReport(
      conforms = report.conforms(),
      validationResults = validationResults
    )
  }

  def validate(dataModel: Model, shapesModelPath: String, reportPath: Option[String], shapeUri: String): Option[ValidationReport] = {
    def performValidation(validationEngine: ValidationEngine): Try[Resource] = Try {
      validationEngine.applyEntailments()
      validationEngine.validateAll
    }

    val shapesModel = JenaUtil.createDefaultModel
    shapesModel.read(shapesModelPath)
    val validationEngine = createTopBraidValidationEngine(dataModel, shapesModel, shapeUri)
    performValidation(validationEngine) match {
      case Success(reportResource) =>
        val reportModel = reportResource.getModel
        reportPath match {
          case Some(path) => writeReport(path, reportResource, Some(shapeUri))
          case _ =>
        }
        Some(populateLeanValidationReport(reportModel))
      case Failure(e) =>
        logger.error(s"Could not validate with shapes model using $shapeUri.", e)
        None
    }
  }

  /**
    * This method is mostly a re-implementation of [[org.topbraid.shacl.engine.ShapesGraph#getRootShapes()]].
    * Unfortunately, we cannot call `shapesGraph.getRootShapes.asScala.toList` directly, since the original method also sets the root shapes and thereby applies the filter.
    * Thus, we would filter out already the initial root shapes.
    *
    * @param shapesModel the shapes model
    * @param shapesGraph the shapes graph
    * @return a list of root shapes
    */
  private def getRootShapes(shapesModel: Model, shapesGraph: ShapesGraph): List[Shape] = {
    val candidates: List[Resource] =
      shapesModel.listSubjectsWithProperty(SH.target).asScala.toList ::: shapesModel.listSubjectsWithProperty(SH.targetClass).asScala.toList ::: shapesModel.listSubjectsWithProperty(SH.targetNode).asScala.toList ::: shapesModel.listSubjectsWithProperty(SH.targetObjectsOf).asScala.toList :::
        shapesModel.listSubjectsWithProperty(SH.targetSubjectsOf).asScala.toList :::
        JenaUtil.getAllInstances(shapesModel.getResource(SH.NodeShape.getURI())).asScala.toList.filter(JenaUtil.hasIndirectType(_, RDFS.Class)) :::
        JenaUtil.getAllInstances(shapesModel.getResource(SH.PropertyShape.getURI())).asScala.toList.filter(JenaUtil.hasIndirectType(_, RDFS.Class))

    candidates.map { s =>
      shapesGraph.getShape(SHFactory.asShape(s).asNode())
    }
  }

  /**
    * This method is a proxy method for [[org.topbraid.shacl.validation.ValidationUtil#createValidationEngine(org.apache.jena.rdf.model.Model, org.apache.jena.rdf.model.Model, org.topbraid.shacl.validation.ValidationEngineConfiguration)]] to hook in our custom shapes filter, i.e., org.sba_research.utils.ShapeFilterByUri.
    *
    * @param dataModel     the data model
    * @param shapesModel   the shapes model
    * @param shapeUri      the URI of the shape to filter
    * @param configuration the configuration
    * @return a SHACL validation engine
    */
  private def createTopBraidValidationEngine(dataModel: Model, shapesModel: Model, shapeUri: String, configuration: ValidationEngineConfiguration = new ValidationEngineConfiguration().setValidateShapes(true)): ValidationEngine = {
    val sm = ValidationUtil.ensureToshTriplesExist(shapesModel)
    SHACLFunctions.registerFunctions(sm)
    val shapesGraphURI = SHACLUtil.createRandomShapesGraphURI()
    val dataset = ARQFactory.get().getDataset(dataModel)
    dataset.addNamedModel(shapesGraphURI.toString(), sm)
    //val config = Config() // TODO: REMOVE
    //PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Creation of Shapes Graph $shapeUri START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
    val shapesGraph = new ShapesGraph(sm)

    //PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Creation of Shapes Graph $shapeUri END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
    //val rootShapes = duplicateShapesGraphWithAllRootShapes.getRootShapes.asScala.toList
    shapesGraph.setShapeFilter(ShapeFilterByUri(getRootShapes(shapesModel, shapesGraph), shapeUri))
    //PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Creation of validation engine $shapeUri START."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
    val engine = ValidationEngineFactory.get().create(dataset, shapesGraphURI, shapesGraph, null)
    //PerformanceMeasurement.writeElapsedTimeToFile(Some(s"[Main] Creation of validation engine $shapeUri END."), System.nanoTime(), config.debugConfig.outputPathPerformanceReport)
    engine.setConfiguration(configuration)
    /*
     classesCache.foreach { c =>
       engine.setClassesCache(c)
       logger.info("Setting cache {}", c.toString)
     }
     */
    engine
  }

  @Deprecated
  def validate3(infModel: InfModel, shapesModelPath: String, reportPath: Option[String], shapeUri: String): Some[ValidationReport] = {
    val shapes = Shapes.parse(shapesModelPath)
    val report = ShaclValidator.get().validate(shapes.getGraph, infModel.getGraph)
    val validationResults = report.getEntries.asScala.toList.map { entry =>
      ShaclValidationResult(
        focusNode = entry.focusNode().toString,
        resultMessage = entry.message(),
        resultPath = Option(entry.resultPath()).map(_.toString),
        sourceShape = entry.source().toString,
        value = Option(entry.value()).map(_.toString())
      )
    }
    val r = ValidationReport(
      conforms = report.conforms(),
      validationResults = validationResults
    )
    Some(r)
  }

  @Deprecated
  def validate2(infModel: InfModel, shapesModelPath: String, reportPath: Option[String], shapeUri: String): ValidationReport = {
    val shapes = Shapes.parse(shapesModelPath)

    val report = shapes.getRootShapes.asScala.find(_.getShapeNode.getURI == shapeUri).map { shape =>
      logger.info(s"Validating shape $shape.")
      logger.info(shape.getShapeGraph.toString)

      ShaclValidator.get().validate(shape.getShapeGraph, infModel.getGraph)
    }.getOrElse(throw new IllegalArgumentException(s"Could not find shape URI $shapeUri in shapes model."))
    val validationResults = report.getEntries.asScala.toList.map { entry =>
      ShaclValidationResult(
        focusNode = entry.focusNode().toString,
        resultMessage = entry.message(),
        resultPath = Option(entry.resultPath()).map(_.toString),
        sourceShape = entry.source().toString,
        value = Option(entry.value()).map(_.toString())
      )
    }
    ValidationReport(
      conforms = report.conforms(),
      validationResults = validationResults
    )
  }

  @Deprecated
  def validateUsingValidationUtil(infModel: InfModel, shapesModelPath: String, reportPath: Option[String]): ValidationReport = {
    val shapeModel = JenaUtil.createDefaultModel
    shapeModel.read(shapesModelPath)
    //ExecStatisticsManager.get().setRecording(true)
    val reportResource = ValidationUtil.validateModel(infModel, shapeModel, true)
    //ExecStatisticsManager.get().getStatistics.forEach(s => logger.info(s"label=${s.getLabel} queryText=${s.getQueryText} node=${s.getContext} duration=${s.getDuration}"))
    val reportModel = reportResource.getModel
    reportPath match {
      case Some(path) => writeReport(path, reportResource)
      case _ =>
    }
    populateValidationReport(reportModel)
  }

  def populateValidationReport(reportModel: Model): ValidationReport = {
    logger.info(ModelPrinter.get.print(reportModel))
    val conforms = reportModel
      .listObjectsOfProperty(SH.conforms).asScala.nextOption()
      .getOrElse(throw new IllegalStateException("Could not retrieve 'conforms' value from report."))
      .asLiteral().getBoolean
    logger.debug(s"Conforms = $conforms.")
    val resultNodeIterator = reportModel.listObjectsOfProperty(SH.result)
    val validationResults =
      resultNodeIterator.asScala.toList.map { x =>
        ShaclValidationResult(
          focusNode = x.asResource().getProperty(SH.focusNode).getObject.toString,
          resultMessage = x.asResource().getProperty(SH.resultMessage).getObject.toString,
          resultPath = Option(x.asResource().getProperty(SH.resultPath)).map(_.getObject.toString),
          sourceShape = x.asResource().getProperty(SH.sourceShape).getObject.toString,
          value = Option(x.asResource().getProperty(SH.value)).map(_.getObject.toString),
        )
      }

    validationResults.foreach(x => logger.debug(s"Retrieved ${x.toString}."))

    ValidationReport(conforms = conforms, validationResults = validationResults)
  }

  def populateLeanValidationReport(reportModel: Model): ValidationReport = {
    val conforms = reportModel
      .listObjectsOfProperty(SH.conforms).asScala.nextOption()
      .getOrElse(throw new IllegalStateException("Could not retrieve 'conforms' value from report."))
      .asLiteral().getBoolean
    logger.debug(s"Conforms = $conforms.")
    val resultNodeIterator = reportModel.listObjectsOfProperty(SH.result)
    val validationResults =
      resultNodeIterator.asScala.toList.map { x =>
        ShaclValidationResult(
          focusNode = x.asResource().getProperty(SH.focusNode).getObject.toString,
          resultMessage = "", // Intentionally left empty
          resultPath = None, // Intentionally left empty
          sourceShape = x.asResource().getProperty(SH.sourceShape).getObject.toString,
          value = None, // Intentionally left empty
        )
      }
    validationResults.foreach(x => logger.debug(s"Retrieved ${x.toString}."))
    ValidationReport(conforms = conforms, validationResults = validationResults)
  }

  def processReasonerValidityReports(validityReports: List[ValidityReport.Report]): ValidationReport =
    ValidationReport(
      conforms = false,
      validityReports.map(r => ReasonerValidationResult(r.`type`, r.isError, r.description))
    )

  private def writeReport(outputPathReport: String, reportResource: Resource, shapeUri: Option[String] = None): Unit = {
    val path = shapeUri match {
      case Some(s) =>
        s.split("#").toList.lift(1).map { shapeUriPostfix =>
          val outputPathExtension = outputPathReport.split("\\.").toList.last
          val outputPathWithoutExtension = outputPathReport.dropRight(outputPathReport.length - outputPathReport.lastIndexOf("."))
          s"${outputPathWithoutExtension}_${shapeUriPostfix}.${outputPathExtension}"
        }.getOrElse(outputPathReport)
      case None => outputPathReport
    }
    logger.debug(s"Writing validation report to $path.")
    val reportFile = new File(path)
    val createFile: Try[Boolean] = Try {
      reportFile.createNewFile
    }
    createFile match {
      case Success(_) =>
        logger.debug(s"Successfully created report file.")
        val reportOutputStream: Try[FileOutputStream] = Try {
          new FileOutputStream(reportFile)
        }
        reportOutputStream match {
          case Success(stream) =>
            RDFDataMgr.write(stream, reportResource.getModel, RDFFormat.TTL)
            logger.debug(s"Wrote report file to $path.")
          case Failure(t) => logger.error(s"$t")
        }
      case Failure(t) => logger.error(s"Failed to create report file: $t")
    }
  }

}

class ValidationResultSerializer extends JsonSerializer[ValidationResult] {
  val logger = Logger(getClass)

  override def serialize(
                          v: ValidationResult,
                          json: JsonGenerator,
                          provider: SerializerProvider
                        ): Unit = {
    json.writeStartObject()
    v match {
      case s: ShaclValidationResult =>
        json.writeFieldName("focus_node")
        json.writeString(s.focusNode)
        // focusNode: String, resultMessage: String, resultPath: Option[String] = None, sourceShape: String, value: Option[String] = None
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
}

class ValidationResultDeserializer extends JsonDeserializer[ValidationResult] {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext): ValidationResult = {
    import com.fasterxml.jackson.databind.JsonNode
    val node: JsonNode = p.getCodec.readTree(p)
    if (node.has("focus_node")) { // Check if we need to deserialize a SHACL Result
      val focusNodeField = node.get("focus_node")
      val focusNode = focusNodeField.asText()
      val resultMessageField = node.get("result_message")
      val resultMessage = resultMessageField.asText()
      val resultPathField = Option(node.get("result_path"))
      val resultPath = resultPathField.map(_.asText)
      val sourceShapeField = node.get("source_shape")
      val sourceShape = sourceShapeField.asText()
      val valueField = Option(node.get("value"))
      val value = valueField.map(_.asText)
      ShaclValidationResult(focusNode, resultMessage, resultPath, sourceShape, value)
    } else if (node.has("type")) { // Reasoner result
      val typeField = node.get("type")
      val `type` = typeField.asText()
      val isErrorField = node.get("is_error")
      val isError = isErrorField.asBoolean()
      val descriptionField = node.get("description")
      val description = descriptionField.asText
      ReasonerValidationResult(`type` = `type`, isError, description)
    }
    else throw new IllegalStateException("Unknown validation result instance.")
  }
}
