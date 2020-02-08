package org.amlsec

import com.typesafe.config.ConfigFactory

case class Config(amlConfig: AmlConfig, secOntConfig: OntConfig, icsSecOntConfig: OntConfig, engValFileName: String, secValFileName: String, outputPathEngValReport: String, outputPathSecValReport: String)

case class AmlConfig(fileName: String, nsOnt: String, nsImp: String)

case class OntConfig(fileName: String, ns: String)

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

    val engValFileName = conf.getString("validation.eng.fileName")
    val secFileName = conf.getString("validation.sec.fileName")
    val outputPathEngValReport = conf.getString("outputPathEngValReport")
    val outputPathSecValReport = conf.getString("outputPathSecValReport")

    this (
      AmlConfig(amlFileName, amlNsOnt, amlNsImp),
      OntConfig(secOntFileName, secOntNs),
      OntConfig(icsSecOntFileName, icsSecOntNs),
      engValFileName,
      secFileName,
      outputPathEngValReport,
      outputPathSecValReport
    )
  }

}