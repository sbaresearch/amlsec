package org.sba_research

import java.io.{File, FileOutputStream}

import com.typesafe.scalalogging.Logger
import org.apache.jena.rdf.model.{InfModel, Model, Resource}
import org.apache.jena.riot.{RDFDataMgr, RDFFormat}
import org.sba_research.model.OntModels
import org.topbraid.jenax.util.JenaUtil
import org.topbraid.shacl.util.ModelPrinter
import org.topbraid.shacl.validation.ValidationUtil
import org.topbraid.shacl.vocabulary.SH

import collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class ValidationReport(conforms: Boolean, validationResults: List[ValidationResult])

case class ValidationResult(focusNode: String, resultMessage: String, resultPath: Option[String] = None, sourceShape: String, value: Option[String] = None)

object Validator {

  val logger = Logger(getClass)

  val shaclNs = "http://www.w3.org/ns/shacl"

  def validate(config: Config, ontModels: OntModels, shapeModelPath: String, reportPath: Option[String]): ValidationReport = {
    val infModel = OntModelUtils.getInfModel(ontModels.aml)
    validate(ontModels = ontModels, infModel = infModel, shapeModelPath = shapeModelPath, reportPath)
  }

  def validate(config: Config, ontModels: OntModels, infModel: InfModel, shapeModelPath: String, reportPath: Option[String]): ValidationReport =
    validate(ontModels = ontModels, infModel = infModel, shapeModelPath = shapeModelPath, reportPath)

  private def validate(ontModels: OntModels, infModel: InfModel, shapeModelPath: String, reportPath: Option[String]): ValidationReport = {
    val shapeModel = JenaUtil.createDefaultModel
    shapeModel.read(shapeModelPath)

    val reportResource = ValidationUtil.validateModel(infModel, shapeModel, true)
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
        ValidationResult(
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

  private def writeReport(outputPathReport: String, reportResource: Resource): Unit = {
    logger.debug(s"Writing validation report to $outputPathReport.")
    val reportFile = new File(outputPathReport)
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
            logger.debug(s"Wrote report file to $outputPathReport.")
          case Failure(t) => logger.error(s"$t")
        }
      case Failure(t) => logger.error(s"Failed to create report file: $t")
    }
  }


}
