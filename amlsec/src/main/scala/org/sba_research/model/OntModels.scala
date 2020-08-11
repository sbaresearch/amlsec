package org.sba_research.model

import org.apache.jena.ontology.OntModel
import org.sba_research.Config
import org.sba_research.utils.OntModelUtils

case class OntModels(aml: OntModel, sec: OntModel, icssec: OntModel, ag: OntModel)

object OntModels {

  def apply(config: Config): OntModels = {

    OntModelUtils.setupLocationMappings(config)

    val amlOntModel = OntModelUtils.createModel(config.amlConfig.ontFilePath, "aml", Some("Turtle"))
    val secOntModel = OntModelUtils.createModel(config.secOntConfig.filePath, "sec")
    val icsSecOntModel = OntModelUtils.createModel(config.icsSecOntConfig.filePath, "icssec")
    val agOntModel = OntModelUtils.createModel(config.agOnt.filePath, "ag")

    this (amlOntModel, secOntModel, icsSecOntModel, agOntModel)
  }
}