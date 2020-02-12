package org.sba_research

import com.typesafe.config.ConfigFactory

case class Config(amlConfig: AmlConfig, secOntConfig: OntConfig, icsSecOntConfig: OntConfig, agOnt: OntConfig, engValFileName: String, secValFileName: String, outputPathEngValReport: String, outputPathSecValReport: String, agConfig: AGConfig)

case class AmlConfig(fileName: String, nsOnt: String, nsImp: String)

case class OntConfig(fileName: String, ns: String)

case class AGConfig(fullPath: String, prunedPath: String, agShortestPath: String)

object Config {

  def apply(): Config = {
    val conf = ConfigFactory.load()

    val amlFileName = conf.getString("aml.fileName")
    val amlNsOnt = conf.getString("aml.nsOnt")
    val amlNsImp = conf.getString("aml.nsImp")

    val secOntFileName = conf.getString("secOnt.fileName")
    val secOntNs = conf.getString("secOnt.ns")

    val icsSecOntFileName = conf.getString("icsSecOnt.fileName")
    val icsSecOntNs = conf.getString("icsSecOnt.ns")

    val agOntFileName = conf.getString("agOnt.fileName")
    val agOntNs = conf.getString("agOnt.ns")

    val engValFileName = conf.getString("validation.eng.fileName")
    val secFileName = conf.getString("validation.sec.fileName")
    val outputPathEngValReport = conf.getString("outputPathEngValReport")
    val outputPathSecValReport = conf.getString("outputPathSecValReport")

    val agFullPath = conf.getString("ag.full.path")
    val agPrunedPath = conf.getString("ag.pruned.path")
    val agShortestPath = conf.getString("ag.shortestPath.path")

    this (
      AmlConfig(amlFileName, amlNsOnt, amlNsImp),
      OntConfig(secOntFileName, secOntNs),
      OntConfig(icsSecOntFileName, icsSecOntNs),
      OntConfig(agOntFileName, agOntNs),
      engValFileName,
      secFileName,
      outputPathEngValReport,
      outputPathSecValReport,
      AGConfig(agFullPath, agPrunedPath, agShortestPath)
    )
  }

}