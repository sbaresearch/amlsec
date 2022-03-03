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
                  qualOntConfig: OntConfig,
                  engValFilePath: String,
                  secValFilePath: String,
                  outputPathEngValReport: String,
                  outputPathSecValReport: String,
                  agConfig: AGConfig,
                  sfcConfig: SfcConfig,
                  qopnConfig: QOPNConfig)

case class AmlConfig(filePath: String, ontFilePath: Option[String], nsOnt: String, nsImp: String)

case class OntConfig(filePath: String, ns: String)

case class AGConfig(fullPath: String, prunedPath: String, agShortestPath: String)

case class SfcTransformationOntConfig(ns: String)

case class SfcConfig(sfcFilePath: String, ontoPlcConfig: OntConfig, sfcTransformationOntConfig: SfcTransformationOntConfig)

case class QOPNConfig(lolaConfig: LoLAConfig, pnmlFilePath: String)

case class DebugConfig(writeKb: Boolean,
                       outputPathAmlsecKb: String,
                       writePerformanceReport: Boolean,
                       outputPathPerformanceReport: String)

case class FusekiConfig(uri: String, dbPath: String)

case class LoLAConfig(filePath: String, stateFilePath: String, pathFilePath: String, outputFilePath: String)

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

    val qualOntFilePath = conf.getString("qualityOnt.filePath")
    val qualOntNs = conf.getString("qualityOnt.ns")

    val sfcFilePath = conf.getString("sfc.sfcFilePath")
    val ontoPlcFilePath = conf.getString("sfc.ontoPlc.filePath")
    val ontoPlcNs = conf.getString("sfc.ontoPlc.ns")
    val sfcTransformationOntNs = conf.getString("sfc.sfcTransformationOnt.ns")

    val lolaFilePath = conf.getString("qopn.lola.filePath")
    val lolaStateFilePath = conf.getString("qopn.lola.stateFilePath")
    val lolaPathFilePath = conf.getString("qopn.lola.pathFilePath")
    val lolaOutputFilePath = conf.getString("qopn.lola.outputFilePath")
    val pnmlFilePath = conf.getString("qopn.pnml.filePath")

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
      OntConfig(qualOntFilePath, qualOntNs),
      engValFilePath,
      secFilePath,
      outputPathEngValReport,
      outputPathSecValReport,
      AGConfig(agFullPath, agPrunedPath, agShortestPath),
      SfcConfig(
        sfcFilePath,
        OntConfig(ontoPlcFilePath, ontoPlcNs),
        SfcTransformationOntConfig(sfcTransformationOntNs)
      ),
      QOPNConfig(LoLAConfig(lolaFilePath, lolaStateFilePath, lolaPathFilePath, lolaOutputFilePath), pnmlFilePath)
    )
  }

}