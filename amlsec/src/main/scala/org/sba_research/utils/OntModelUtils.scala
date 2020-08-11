package org.sba_research.utils

import java.io.{BufferedWriter, File, FileInputStream, FileWriter}

import com.typesafe.scalalogging.Logger
import org.apache.jena.ontology._
import org.apache.jena.rdf.model.{InfModel, Model, ModelFactory}
import org.apache.jena.reasoner.rulesys.{OWLFBRuleReasonerFactory, OWLMicroReasonerFactory, OWLMiniReasonerFactory, RDFSRuleReasonerFactory}
import org.apache.jena.reasoner.transitiveReasoner.TransitiveReasonerFactory
import org.apache.jena.reasoner.{Reasoner, ReasonerRegistry}
import org.apache.jena.util.iterator.ExtendedIterator
import org.apache.jena.util.{FileManager, LocationMapper, SplitIRI}
import org.sba_research.Config

import scala.annotation.tailrec

object OntModelUtils {

  val logger = Logger(getClass)

  /**
    * Removes the namespace from a given String.
    *
    * @param s the String to remove the namespace from
    * @return a string without the namespace
    */
  def removeNamespace(s: String): String = SplitIRI.splitpoint(s) match {
    case x if x > -1 => s.substring(x)
    case _ => s
  }

  def getInfModel(model: Model, reasonerUri: String): InfModel = {
    // val schema = FileManager.get.loadModel(path.toFile.getAbsolutePath + "/src/main/resources/amlontology.ttl")
    val reasoner: Reasoner = reasonerUri match {
      case TransitiveReasonerFactory.URI => ReasonerRegistry.getTransitiveReasoner
      case RDFSRuleReasonerFactory.URI => ReasonerRegistry.getRDFSReasoner
      case OWLFBRuleReasonerFactory.URI => ReasonerRegistry.getOWLReasoner
      case OWLMicroReasonerFactory.URI => ReasonerRegistry.getOWLMicroReasoner
      case OWLMiniReasonerFactory.URI => ReasonerRegistry.getOWLMiniReasoner
      case _ =>
        logger.error(s"Could not match $reasonerUri to reasoner, falling back to ${OWLFBRuleReasonerFactory.URI}.")
        ReasonerRegistry.getOWLReasoner
    }
    //var reasoner = ReasonerRegistry.getOWLReasoner
    //reasoner = reasoner.bindSchema(model)
    ModelFactory.createInfModel(reasoner, model)
  }

  def write(ontModel: Model, pathName: String): Unit = {
    val file = new File(pathName)
    val bw = new BufferedWriter(new FileWriter(file))
    ontModel.write(bw, "Turtle")
    bw.close()
  }

  def createModel(filePath: String, base: String, lang: Option[String] = None): OntModel = {
    val m = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM)
    val f = new File(filePath)
    val in = new FileInputStream(f)
    lang match {
      case Some(l) => m.read(in, base, l)
      case None => m.read(in, base)
    }
    m.setDynamicImports(true) // Enable import processing
    // cf. https://stackoverflow.com/a/17447438/5107545
    // `individual.listOntClasses(false).asScala.toList` throws
    // org.apache.jena.ontology.ConversionException: Cannot convert node http://www.w3.org/2002/07/owl#NamedIndividual to OntClass: it does not have rdf:type owl:Class or equivalent
    m.setStrictMode(false)
    m
  }

  @tailrec
  def getAllSubclasses(ontCls: OntClass, l: List[OntClass] = List.empty, acc: List[OntClass] = List.empty): List[OntClass] =
    acc match {
      case x :: xs =>
        getAllSubclasses(
          x,
          ontCls :: l, // Prepend currently processed ontology class
          xs ::: getOntResources(x.listSubClasses()) // Use elements on same hierarchy and subclasses of next element as accumulator
            .flatMap(ontClsConversion))
      case Nil if ontCls.hasSubClass && !ontCls.listSubClasses().toList.isEmpty => // Currently processed ontology class is last one in hierarchy, go one level down as subclass exist
        getAllSubclasses(ontCls, l, getOntResources(ontCls.listSubClasses()).flatMap(ontClsConversion))
      case Nil => ontCls :: l // Last element in hierarchy, no subclasses of ontology class
    }

  @tailrec
  def getOntResources(iter: ExtendedIterator[_ <: OntResource], l: List[OntResource] = List.empty): List[OntResource] =
    if (iter.hasNext) getOntResources(iter, l ::: List(iter.next()))
    else l

  private def ontClsConversion(cls: OntResource) = cls match {
    case cls: OntClass => Some(cls)
    case _ => None
  }

  /**
    * Sets up the appropriate mappings from IRIs to local files.
    */
  def setupLocationMappings(config: Config): Unit = {
    val locMgr = new LocationMapper()
    locMgr.addAltEntry(config.secOntConfig.ns, config.secOntConfig.filePath)
    locMgr.addAltEntry(config.icsSecOntConfig.ns, config.icsSecOntConfig.filePath)
    locMgr.addAltEntry(config.agOnt.ns, config.agOnt.filePath)
    FileManager.get.setLocationMapper(locMgr)
    // Ensure that the document manager directly uses the global file manager (see JavaDoc of OntDocumentManager)
    OntDocumentManager.getInstance.setFileManager(FileManager.get)
  }

}