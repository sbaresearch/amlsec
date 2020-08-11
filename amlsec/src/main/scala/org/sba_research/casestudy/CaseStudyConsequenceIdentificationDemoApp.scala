package org.sba_research.casestudy

import com.typesafe.scalalogging.Logger
import org.apache.jena.ontology.OntModel
import org.sba_research.Config
import org.sba_research.model.{AmlOntExtension, OntModels}
import org.sba_research.utils.{OntModelUtils, QueryExecutor}

object CaseStudyConsequenceIdentificationDemoApp {

  def main(args: Array[String]): Unit = {

    val logger = Logger("Root")
    val config = Config()
    // We still have to populate the ontology models in order to setup the alternative location mappings (otherwise the ontologies cannot be found)
    val ontModels = OntModels(config)

    val model: OntModel = OntModelUtils.createModel("generated/amlsec.ttl", "aml", Some("ttl"))
    AmlOntExtension.importSecOnts(config, model)
    model.setStrictMode(false)

    QueryExecutor.query(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX amlImp: <http://www.ipr.kit.edu/aml_importer#>
          |PREFIX amlOnt: <http://www.ipr.kit.edu/aml_ontology#>
          |PREFIX secOnt: <http://securityontology.com/secont#>
          |PREFIX icsSecOnt: <http://securityontology.com/icssecont#>
          |SELECT DISTINCT ?actuator ?vulnerableDevice1 ?vulnerabilityOfDevice1 ?vulnerableDevice2 ?vulnerabilityOfDevice2
          |      WHERE {
          |          ?hazardConsequence1 rdf:type/rdfs:subClassOf* icsSecOnt:Hazard .
          |          ?vulnerableDevice1 secOnt:asset_impactedBy_Consequence ?hazardConsequence1 ;
          |                             secOnt:asset_has_Vulnerability ?vulnerabilityOfDevice1 ;
          |                             amlOnt:hasIE/amlOnt:hasEI ?socket1 .
          |
          |          ?motorWireConnection a amlImp:MotorWire ;
          |                               amlOnt:hasEI ?plug1, ?plug2 .
          |
          |          ?plug1 amlOnt:hasRefPartner ?link1 .
          |          ?plug2 amlOnt:hasRefPartner ?link2 .
          |          ?socket1 amlOnt:hasRefPartner ?link1 .
          |          ?socket2 amlOnt:hasRefPartner ?link2 .
          |
          |          FILTER ( ?plug1 != ?socket1 ) .
          |          FILTER ( ?plug2 != ?socket2 ) .
          |
          |          ?actuator amlOnt:hasIE/amlOnt:hasEI ?socket2 .
          |
          |          FILTER ( ?vulnerableDevice1 != ?actuator ).
          |
          |          OPTIONAL {
          |              ?logicalConnection a amlImp:LogicalConnection ;
          |                                 amlOnt:hasEI ?plug3, ?plug4 .
          |
          |              FILTER ( str(?plug3) < str(?plug4) ) .
          |
          |              ?plug3 amlOnt:hasRefPartner ?link3 .
          |              ?plug4 amlOnt:hasRefPartner ?link4 .
          |              ?socket3 amlOnt:hasRefPartner ?link3 .
          |              ?socket4 amlOnt:hasRefPartner ?link4 .
          |              FILTER ( ?plug3 != ?socket3 ) .
          |              FILTER ( ?plug4 != ?socket4 ) .
          |
          |              ?vulnerableDevice1 amlOnt:hasIE/amlOnt:hasEI ?socket3 .
          |              ?vulnerableDevice2 amlOnt:hasIE/amlOnt:hasEI ?socket4 .
          |
          |              ?hazardConsequence2 rdf:type/rdfs:subClassOf* icsSecOnt:Hazard .
          |              ?vulnerableDevice2 secOnt:asset_impactedBy_Consequence ?hazardConsequence2 ;
          |                                 secOnt:asset_has_Vulnerability ?vulnerabilityOfDevice2 .
          |          }
          |       }
          """.stripMargin,
      model = model,
      resBinding = None
    )
  }

}
