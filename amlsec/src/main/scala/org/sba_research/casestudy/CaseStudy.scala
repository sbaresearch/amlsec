package org.sba_research.casestudy

import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.ontology.OntModel
import org.apache.jena.rdf.model.ResourceFactory
import org.sba_research.Config
import org.sba_research.ag.AttackGraph

object CaseStudy {

  /**
    * Plots a CPAG with manually adjusted weights of control devices to focus on a specific part of the spot welding process.
    *
    * @param config the configurations
    * @param model  the ontology model
    */
  def plotCpag(config: Config, model: OntModel, withInference: Boolean = true): Unit = {
    val vertexHasWeightUri = s"${config.agOnt.ns}#vertex_has_Weight"
    val vertexHasWeightProperty = Option(model.getObjectProperty(vertexHasWeightUri)).getOrElse(model.getDatatypeProperty(vertexHasWeightUri))

    val indvKRC4_2 = model.getIndividual(s"${config.agOnt.ns}#vertex_ie_KRC4_2_483b250d-bc7e-411d-976c-9dea138fbc60")
    indvKRC4_2.setPropertyValue(vertexHasWeightProperty, ResourceFactory.createTypedLiteral("9.0", XSDDatatype.XSDdouble))
    val indvS71510_2 = model.getIndividual(s"${config.agOnt.ns}#vertex_ie_SimaticS71510SPF1PN2_3cf68364-1144-4262-b3c6-b050303c5ef2")
    indvS71510_2.setPropertyValue(vertexHasWeightProperty, ResourceFactory.createTypedLiteral("9.0", XSDDatatype.XSDdouble))
    val indvS7_2 = model.getIndividual(s"${config.agOnt.ns}#vertex_ie_SimaticS71516F_2_c403dd71-4d52-4d22-a917-e2a991008be8")
    indvS7_2.setPropertyValue(vertexHasWeightProperty, ResourceFactory.createTypedLiteral("9.0", XSDDatatype.XSDdouble))

    AttackGraph.plotPrunedAg(config, model, withInference)
  }

}
