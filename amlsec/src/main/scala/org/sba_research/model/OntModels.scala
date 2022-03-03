package org.sba_research.model

import org.apache.jena.ontology.OntModel
import org.sba_research.Config
import org.sba_research.utils.OntModelUtils

case class OntModels(aml: Option[OntModel], sec: OntModel, icssec: OntModel, ag: OntModel, quality: OntModel)

object OntModels {

  def apply(config: Config): OntModels = {

    OntModelUtils.setupLocationMappings(config)

    val amlOntModel = config.amlConfig.ontFilePath.map(p => OntModelUtils.createModel(p, "aml", Some("Turtle")))
    val secOntModel = OntModelUtils.createModel(config.secOntConfig.filePath, "sec")
    val icsSecOntModel = OntModelUtils.createModel(config.icsSecOntConfig.filePath, "icssec")
    val agOntModel = OntModelUtils.createModel(config.agOnt.filePath, "ag")
    val qualOntModel = OntModelUtils.createModel(config.qualOntConfig.filePath, "qual")

    this (amlOntModel, secOntModel, icsSecOntModel, agOntModel, qualOntModel)
  }
}