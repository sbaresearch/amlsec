package org.amlsec

import com.typesafe.scalalogging.Logger
import org.amlsec.model.OntModels

object MainApp {

  def main(args: Array[String]): Unit = {

    val logger = Logger("Root")
    val config = Config()
    val ontModels = OntModels(config)
    val augmentedOntModels = AmlOntExtension.augment(config, ontModels.aml) map (aml => OntModels(aml = aml, sec = ontModels.sec, icssec = ontModels.icssec))

    augmentedOntModels map { models =>
      /* Perform validation of engineering artifact. */
      Validator.validate(
        config = config,
        ontModels = models,
        shapeModelPath = config.engValFileName,
        reportPath = Some(config.outputPathEngValReport)
      )
    }
    val infModel = augmentedOntModels.map(models => OntModelUtils.getInfModel(models.aml))

    // val preloadedReportModel = RDFDataMgr.loadModel(getClass.getResource("/reports/report_sec_val.ttl").toString)
    val vulnerabilityReport = for {
      ontM <- augmentedOntModels
      infM <- infModel
    } yield Validator.validate(
      config = config,
      ontModels = ontM,
      infModel = infM,
      shapeModelPath = config.secValFileName,
      reportPath = Some(config.outputPathSecValReport)
    )
    val vulnerabilityInstantiatedModels =
      for {
        // report <- Some(Validator.populateValidationReport(preloadedReportModel)) // vulnerabilityReport
        report <- vulnerabilityReport
        augMo <- augmentedOntModels
        infM <- infModel
        vulnMo <- VulnerabilityModeling.processReport(config, augMo, infM, report)
      } yield vulnMo


    val cveVulnModels = vulnerabilityInstantiatedModels flatMap { models =>
      VulnerabilityModeling.processCves(config, models, CveChecker.check(amlOntModel = models.aml))
    }

    // Write knowledge base to file
    cveVulnModels foreach { m => OntModelUtils.write(m.aml, "src/main/resources/amlsec.ttl") }

  }

}
