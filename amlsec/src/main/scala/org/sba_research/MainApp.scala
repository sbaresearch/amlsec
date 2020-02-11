package org.sba_research

import com.typesafe.scalalogging.Logger
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.ontology.OntModel
import org.apache.jena.rdf.model.ResourceFactory
import org.sba_research.model.OntModels

object MainApp {

  def main(args: Array[String]): Unit = {

    val logger = Logger("Root")
    val config = Config()
    val ontModels = OntModels(config)
    val augmentedOntModels = AmlOntExtension.augment(config, ontModels.aml) map (aml => OntModels(aml = aml, sec = ontModels.sec, icssec = ontModels.icssec, ag = ontModels.ag))

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

    val agModels = cveVulnModels flatMap { models =>
      AttackGraph.generate(config, models.aml)
        .map(m => OntModels(aml = models.aml.add(m).asInstanceOf[OntModel], sec = ontModels.sec, icssec = ontModels.icssec, ag = ontModels.ag))
    }

    agModels foreach { m => OntModelUtils.write(m.aml, "src/main/resources/amlsec.ttl") }

    // val model: OntModel = OntModelUtils.createModel("amlsec.ttl", "aml", Some("ttl"))
    // AmlOntExtension.importSecOnts(config, model)
    // model.setStrictMode(false)

    // Manually adjust weights of control devices to focus on a specific part of the spot welding process
    agModels foreach { m =>
      val vertexHasWeightProperty = m.aml.getObjectProperty(s"${config.agOnt.ns}#vertex_has_Weight")
      val indvKRC4_2 = m.aml.getIndividual(s"${config.agOnt.ns}#vertex_ie_KRC4_2_483b250d-bc7e-411d-976c-9dea138fbc60")
      indvKRC4_2.setPropertyValue(vertexHasWeightProperty, ResourceFactory.createTypedLiteral("9.0", XSDDatatype.XSDdouble))
      val indvS71510_2 = m.aml.getIndividual(s"${config.agOnt.ns}#vertex_ie_SimaticS71510SPF1PN2_3cf68364-1144-4262-b3c6-b050303c5ef2")
      indvS71510_2.setPropertyValue(vertexHasWeightProperty, ResourceFactory.createTypedLiteral("9.0", XSDDatatype.XSDdouble))
      val indvS7_2 = m.aml.getIndividual(s"${config.agOnt.ns}#vertex_ie_SimaticS71516F_2_c403dd71-4d52-4d22-a917-e2a991008be8")
      indvS7_2.setPropertyValue(vertexHasWeightProperty, ResourceFactory.createTypedLiteral("9.0", XSDDatatype.XSDdouble))

      AttackGraph.plotPrunedAg(config, m.aml)
    }

  }

}
