package org.sba_research


import java.io.{BufferedWriter, File, FileWriter}

import org.apache.jena.ontology.{OntClass, OntModel, OntModelSpec, OntResource}
import org.apache.jena.rdf.model.{InfModel, ModelFactory}
import org.apache.jena.reasoner.ReasonerRegistry
import org.apache.jena.util.SplitIRI
import org.apache.jena.util.iterator.ExtendedIterator

import scala.annotation.tailrec

object OntModelUtils {

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

  def getInfModel(ontModel: OntModel): InfModel = {
    // val schema = FileManager.get.loadModel(path.toFile.getAbsolutePath + "/src/main/resources/amlontology.ttl")
    var reasoner = ReasonerRegistry.getOWLReasoner
    reasoner = reasoner.bindSchema(ontModel)
    ModelFactory.createInfModel(reasoner, ontModel)
  }

  def write(ontModel: OntModel, pathName: String): Unit = {
    val file = new File(pathName)
    val bw = new BufferedWriter(new FileWriter(file))
    ontModel.write(bw, "Turtle")
    bw.close()
  }

  def createModel(fileName: String, base: String, lang: Option[String] = None): OntModel = {
    val m = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM)
    val in = getClass.getResourceAsStream(s"/$fileName")
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
      case Nil if ontCls.hasSubClass => // Currently processed ontology class is last one in hierarchy, go one level down as subclass exist
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

}