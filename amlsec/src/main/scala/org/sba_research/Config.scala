package org.sba_research

import com.typesafe.config.ConfigFactory

case class Config(baseDir: String,
                  amlToOwlProgram: String,
                  reasonerUri: String,
                  debugConfig: DebugConfig,
                  fusekiConfig: FusekiConfig,
                  amlConfig: AmlConfig,
                  secOntConfig: OntConfig,
                  icsSecOntConfig: OntConfig,
                  agOnt: OntConfig,
                  engValFilePath: String,
                  secValFilePath: String,
                  outputPathEngValReport: String,
                  outputPathSecValReport: String,
                  agConfig: AGConfig)

case class AmlConfig(filePath: String, ontFilePath: Option[String], nsOnt: String, nsImp: String)

case class OntConfig(filePath: String, ns: String)

case class AGConfig(fullPath: String, prunedPath: String, agShortestPath: String)

case class DebugConfig(writeKb: Boolean,
                       outputPathAmlsecKb: String,
                       writePerformanceReport: Boolean,
                       outputPathPerformanceReport: String)

case class FusekiConfig(uri: String, dbPath: String)

object Config {

  def apply(): Config = {
    val conf = ConfigFactory.load()

    val baseDir = conf.getString("baseDir")
    val amlToOwlProgram = conf.getString("amlToOwlProgram")
    val reasonerUri = conf.getString("reasonerUri")

    val writeKb = conf.getBoolean("debug.kb.writeKb")
    val outputPathAmlsecKb = conf.getString("debug.kb.outputPathAmlsecKb")

    val fusekiUri = conf.getString("fuseki.uri")
    val fusekiDbPath = conf.getString("fuseki.tdbDir")

    val writePerformanceReport = conf.getBoolean("debug.performance.writePerformanceReport")
    val outputPathPerformanceReport = conf.getString("debug.performance.outputPathPerformanceReport")

    val amlFilePath = conf.getString("aml.filePath")
    val amlOntFilePath = if (conf.hasPath("aml.ontFilePath")) Some(conf.getString("aml.ontFilePath")) else None
    val amlNsOnt = conf.getString("aml.nsOnt")
    val amlNsImp = conf.getString("aml.nsImp")

    val secOntFilePath = conf.getString("secOnt.filePath")
    val secOntNs = conf.getString("secOnt.ns")

    val icsSecOntFilePath = conf.getString("icsSecOnt.filePath")
    val icsSecOntNs = conf.getString("icsSecOnt.ns")

    val agOntFilePath = conf.getString("agOnt.filePath")
    val agOntNs = conf.getString("agOnt.ns")

    val engValFilePath = conf.getString("validation.eng.filePath")
    val secFilePath = conf.getString("validation.sec.filePath")
    val outputPathEngValReport = conf.getString("outputPathEngValReport")
    val outputPathSecValReport = conf.getString("outputPathSecValReport")

    val agFullPath = conf.getString("ag.full.path")
    val agPrunedPath = conf.getString("ag.pruned.path")
    val agShortestPath = conf.getString("ag.shortestPath.path")

    this (
      baseDir,
      amlToOwlProgram,
      reasonerUri,
      DebugConfig(writeKb, outputPathAmlsecKb, writePerformanceReport, outputPathPerformanceReport),
      FusekiConfig(fusekiUri, fusekiDbPath),
      AmlConfig(amlFilePath, amlOntFilePath, amlNsOnt, amlNsImp),
      OntConfig(secOntFilePath, secOntNs),
      OntConfig(icsSecOntFilePath, icsSecOntNs),
      OntConfig(agOntFilePath, agOntNs),
      engValFilePath,
      secFilePath,
      outputPathEngValReport,
      outputPathSecValReport,
      AGConfig(agFullPath, agPrunedPath, agShortestPath)
    )
  }

}