package org.amlsec.model


import org.apache.jena.ontology.{OntDocumentManager, OntModel}
import org.apache.jena.util.{FileManager, LocationMapper}
import org.amlsec.{Config, OntModelUtils}

case class OntModels(aml: OntModel, sec: OntModel, icssec: OntModel)

object OntModels {

  def apply(config: Config): OntModels = {

    /**
      * Sets up the appropriate mappings from IRIs to local files.
      */
    def setupLocationMappings(): Unit = {
      val locMgr = new LocationMapper()
      locMgr.addAltEntry(config.secOntConfig.ns, getClass.getResource("/" + config.secOntConfig.fileName).toString)
      locMgr.addAltEntry(config.icsSecOntConfig.ns, getClass.getResource("/" + config.icsSecOntConfig.fileName).toString)
      FileManager.get.setLocationMapper(locMgr)
      // Ensure that the document manager directly uses the global file manager (see JavaDoc of OntDocumentManager)
      OntDocumentManager.getInstance.setFileManager(FileManager.get)
    }

    setupLocationMappings()

    val amlOntModel = OntModelUtils.createModel(config.amlConfig.fileName, "aml", Some("Turtle"))
    val secOntModel = OntModelUtils.createModel(config.secOntConfig.fileName, "sec")
    val icsSecOntModel = OntModelUtils.createModel(config.icsSecOntConfig.fileName, "icssec")

    this (amlOntModel, secOntModel, icsSecOntModel)
  }
}