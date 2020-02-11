package org.sba_research

import org.apache.jena.ontology.{OntClass, OntModel, OntResource}
import org.apache.jena.rdf.model.ResourceFactory
import org.apache.jena.util.iterator.ExtendedIterator
import org.apache.jena.vocabulary.OWL2

object AmlOntExtension {

  def augment(config: Config, ontModel: OntModel): Option[OntModel] =
    for {
      /* TBox and RBox axioms to establish clean semantics */
      ontModelInvPartnerObject <- addInverseOfRefPartnerObjectProperty(config, ontModel) // Add `inverseOf` to `refPartnerSideA` and `refPartnerSideB`
      ontModelHasIEInv <- addInverseOfHasIeEiObjectProperty(config, ontModelInvPartnerObject) // Add inverseOf to `hasIE` and `hasEI`
      ontModelSubClassOf <- addSubClassOfHost(config, ontModelHasIEInv) // Add `Host` SubClass Of to `PLC`, `HMI`
      ontModelFirewallSubClassOf <- addFirewallSubClassOfNetworkDevice(config, ontModelSubClassOf) // Add `NetworkDevice` SubClass Of to `Firewall`
      ontModelLogicalEndpointSameAs <- addEquivForLogicalEndpoint(config, ontModelFirewallSubClassOf) // Add `LogicalEndPoint` equivalent as `LogicalEndpoint`
      /* Further axioms to validate model */
      ontModelValLink <- addLinkValidation(config, ontModelLogicalEndpointSameAs) // Add axioms for link / hasRefPartnerA/B object property validation
      ontModelValPlcProgram <- addPlcProgramDataPropertyValidation(config, ontModelValLink) // Add axioms for PLC program data property validation
      ontModelValOpcUa <- addOpcUaSecPolicyValidation(config, ontModelValPlcProgram) // Add axioms for OPC UA class validation
      ontModelValWire <- addWireValidation(config, ontModelValOpcUa) // Add axioms for Wire class validation
      ontModelValWirelessConnection <- addWirelessConnectionValidation(config, ontModelValWire) // Add axioms for WirelessConnection class validation
      ontModelWireDisjointUnionOf <- addWireDisjointUnionOf(config, ontModelValWirelessConnection) // Add `Disjoint Union of` to `Wire` with all subclasses thereof
      ontModelProtocolDisjointUnionOf <- addProtocolDisjointUnionOf(config, ontModelWireDisjointUnionOf) // // Add `Disjoint Union of` to `Protocol` with all subclasses thereof
      ontModelOPCUASecurityPolicyDisjointUnionOf <- addOPCUASecurityPolicyDisjointUnionOf(config, ontModelProtocolDisjointUnionOf) // // Add `Disjoint Union of` to `OPCUASecurityPolicy` with all subclasses thereof
      /* Import ICS security ontology and security ontology */
      ontModelWithImportedSecOnts <- importSecOnts(config, ontModelOPCUASecurityPolicyDisjointUnionOf)
      /* Add dynamic security know-how */
      ontModelConsequencesIndv <- addConsequencesIndividuals(config, ontModelWithImportedSecOnts) // Create consequence individuals
      /* Add concept equivalence */
      ontModelConceptEquivs <- addConceptEquivalenceAcrossNs(config, ontModelConsequencesIndv)
      /* Add subclass of axioms for assets network device, host, and security device */
      ontModelAssetsSubclasses <- addAssetsSubclasses(config, ontModelConceptEquivs)
      /* Add object properties 'asset_physicallyConnectedTo_Asset' */
      ontModelAssetConnectedToAssetRoles <- addAssetPhysicallyConnectedToAssetRoles(config, ontModelAssetsSubclasses)
      /* Add object properties 'asset_logicallyConnectedTo_Asset' */
      ontModelAssetLogicallyConnectedToAssetRoles <- addAssetLogicallyConnectedToAssetRoles(config, ontModelAssetConnectedToAssetRoles)
    } yield ontModelAssetLogicallyConnectedToAssetRoles


  private def addInverseOfRefPartnerObjectProperty(config: Config, ontModel: OntModel): Option[OntModel] = {
    val refPartnerSideA = ontModel.getObjectProperty(config.amlConfig.nsOnt + "#hasRefPartnerSideA")
    val refPartnerSideB = ontModel.getObjectProperty(config.amlConfig.nsOnt + "#hasRefPartnerSideB")
    val refPartner = ontModel.createObjectProperty(config.amlConfig.nsOnt + "#hasRefPartner")
    refPartner.addInverseOf(refPartnerSideA)
    refPartner.addInverseOf(refPartnerSideB)
    Some(ontModel)
  }

  private def addInverseOfHasIeEiObjectProperty(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasIE = ontModel.getObjectProperty(config.amlConfig.nsOnt + "#hasIE")
    val hasIEInv = ontModel.createObjectProperty(config.amlConfig.nsOnt + "#hasIEInv")
    val hasEI = ontModel.getObjectProperty(config.amlConfig.nsOnt + "#hasEI")
    val hasEIInv = ontModel.createObjectProperty(config.amlConfig.nsOnt + "#hasEIInv")
    hasIE.addInverseOf(hasIEInv)
    hasEI.addInverseOf(hasEIInv)
    Some(ontModel)
  }

  private def addSubClassOfHost(config: Config, ontModel: OntModel): Option[OntModel] = {
    val plcClass = ontModel.getOntClass(config.amlConfig.nsImp + "#PLC")
    val hmiClass = ontModel.getOntClass(config.amlConfig.nsImp + "#HMI")
    val hostClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Host")
    hostClass.addSubClass(plcClass)
    hostClass.addSubClass(hmiClass)
    Some(ontModel)
  }

  private def addFirewallSubClassOfNetworkDevice(config: Config, ontModel: OntModel): Option[OntModel] = {
    val firewallClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Firewall")
    val networkDeviceClass = ontModel.getOntClass(config.amlConfig.nsImp + "#NetworkDevice")
    networkDeviceClass.addSubClass(firewallClass)
    Some(ontModel)
  }

  private def addEquivForLogicalEndpoint(config: Config, ontModel: OntModel): Option[OntModel] = {
    val amlSecLogicalEndpointClass = ontModel.getOntClass(config.amlConfig.nsImp + "#LogicalEndpoint")
    val amlBaseLogicalEndPointClass = ontModel.getOntClass(config.amlConfig.nsImp + "#LogicalEndPoint")
    amlBaseLogicalEndPointClass.addEquivalentClass(amlSecLogicalEndpointClass)
    Some(ontModel)
  }

  private def addWireDisjointUnionOf(config: Config, ontModel: OntModel): Option[OntModel] = {
    val wireClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Wire")
    addDisjointUnionOf(config, ontModel, wireClass, wireClass.listSubClasses())
  }


  private def addProtocolDisjointUnionOf(config: Config, ontModel: OntModel): Option[OntModel] = {
    val wireClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Protocol")
    addDisjointUnionOf(config, ontModel, wireClass, wireClass.listSubClasses())
  }

  private def addOPCUASecurityPolicyDisjointUnionOf(config: Config, ontModel: OntModel): Option[OntModel] = {
    val wireClass = ontModel.getOntClass(config.amlConfig.nsImp + "#OPCUASecurityPolicy")
    addDisjointUnionOf(config, ontModel, wireClass, wireClass.listSubClasses())
  }


  private def addDisjointUnionOf(config: Config, ontModel: OntModel, cls: OntClass, disjointWithClasses: ExtendedIterator[OntClass]): Option[OntModel] = {
    val rdfListDisjointClasses = ontModel.createList(disjointWithClasses)
    ontModel.add(cls, ontModel.createProperty(OWL2.disjointUnionOf.toString), rdfListDisjointClasses)
    Some(ontModel)
  }

  private def addLinkValidation(config: Config, ontModel: OntModel): Option[OntModel] = {
    val refPartner = ontModel.getObjectProperty(config.amlConfig.nsOnt + "#hasRefPartner")
    val linkClassName = "Link"
    val linkClass = ontModel.getOntClass(config.amlConfig.nsImp + "#" + linkClassName)
    refPartner.addRange(linkClass)
    addDisjointWithAllOtherClasses(config, ontModel, linkClassName)
  }

  private def addPlcProgramDataPropertyValidation(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasCopyProtection = ontModel.getDatatypeProperty(config.amlConfig.nsImp + "#hasCopyProtection")
    val hasKnowHowProtection = ontModel.getDatatypeProperty(config.amlConfig.nsImp + "#hasKnowHowProtection")
    val plcProgramClassName = "PLCProgram"
    val plcProgramClass = ontModel.getOntClass(config.amlConfig.nsImp + "#" + plcProgramClassName)
    hasCopyProtection.addDomain(plcProgramClass)
    hasKnowHowProtection.addDomain(plcProgramClass)
    addDisjointWithAllOtherClasses(config, ontModel, plcProgramClassName)
  }

  private def addDisjointWithAllOtherClasses(config: Config, ontModel: OntModel, className: String, excludedClasses: List[OntClass] = List.empty): Option[OntModel] = {
    val cls = ontModel.getOntClass(config.amlConfig.nsImp + "#" + className)
    val classes = OntModelUtils.getOntResources(ontModel.listHierarchyRootClasses()) map { case c: OntClass => c }
    classes.filterNot { x => x == cls || x.hasSubClass(cls) || x.hasSuperClass(cls) || excludedClasses.contains(x) }.foreach(cls.addDisjointWith)
    Some(ontModel)
  }

  private def addOpcUaSecPolicyValidation(config: Config, ontModel: OntModel): Option[OntModel] =
    addDisjointWithAllOtherClasses(config, ontModel, "OPCUASecurityPolicy")

  private def addWireValidation(config: Config, ontModel: OntModel): Option[OntModel] =
    addDisjointWithAllOtherClasses(config, ontModel, "Wire", List(ontModel.getOntClass(config.amlConfig.nsImp + "#Conduit")))

  private def addWirelessConnectionValidation(config: Config, ontModel: OntModel): Option[OntModel] =
    addDisjointWithAllOtherClasses(config, ontModel, "WirelessConnection", List(ontModel.getOntClass(config.amlConfig.nsImp + "#Conduit")))

  private def addConsequencesIndividuals(config: Config, ontModel: OntModel): Option[OntModel] = {

    val conseqProperty = ontModel.getProperty(config.secOntConfig.ns + "#consequence_impacts_Asset")
    val ipBreachClass = ontModel.getOntClass(config.secOntConfig.ns + "#IPBreach")
    val dataBreachClass = ontModel.getOntClass(config.secOntConfig.ns + "#DataBreach")
    val regulatoryNonComplianceClass = ontModel.getOntClass(config.secOntConfig.ns + "#RegulatoryNonCompliance")
    val hazardClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Hazard")
    val businessInterruptionClass = ontModel.getOntClass(config.secOntConfig.ns + "#BusinessInterruption")


    def createConseqIndv(individual: OntResource, ontClass: OntClass) = {
      val n = ontModel.createIndividual(ontClass.getURI + "_" + OntModelUtils.removeNamespace(individual.getURI), ontClass)
      n.addProperty(conseqProperty, individual)
    }

    val programSbcls = OntModelUtils.getAllSubclasses(ontModel.getOntClass(config.amlConfig.nsImp + "#Program"))
    programSbcls.foreach { cls => OntModelUtils.getOntResources(cls.listInstances()) foreach { i => createConseqIndv(i, ipBreachClass) } }

    val historianClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Historian")
    OntModelUtils.getOntResources(historianClass.listInstances()) foreach { i =>
      createConseqIndv(i, dataBreachClass)
      createConseqIndv(i, regulatoryNonComplianceClass)
    }

    val sisClass = ontModel.getOntClass(config.amlConfig.nsImp + "#SIS")
    OntModelUtils.getOntResources(sisClass.listInstances()) foreach { i => createConseqIndv(i, hazardClass) }

    val mesClass = ontModel.getOntClass(config.amlConfig.nsImp + "#MES")
    OntModelUtils.getOntResources(mesClass.listInstances()) foreach { i => createConseqIndv(i, businessInterruptionClass) }

    val mailServerClass = ontModel.getOntClass(config.amlConfig.nsImp + "#MailServer")
    OntModelUtils.getOntResources(mailServerClass.listInstances()) foreach { i => createConseqIndv(i, dataBreachClass) }

    val fileServerClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Fileserver")
    OntModelUtils.getOntResources(fileServerClass.listInstances()) foreach { i =>
      createConseqIndv(i, ipBreachClass)
      createConseqIndv(i, dataBreachClass)
    }

    val erpClass = ontModel.getOntClass(config.amlConfig.nsImp + "#ERP")
    OntModelUtils.getOntResources(erpClass.listInstances()) foreach { i => createConseqIndv(i, businessInterruptionClass) }

    val plcClass = ontModel.getOntClass(config.amlConfig.nsImp + "#PLC")
    OntModelUtils.getOntResources(plcClass.listInstances()) foreach { i =>
      createConseqIndv(i, businessInterruptionClass)
      createConseqIndv(i, hazardClass)
    }

    val hmiClass = ontModel.getOntClass(config.amlConfig.nsImp + "#HMI")
    OntModelUtils.getOntResources(hmiClass.listInstances()) foreach { i =>
      createConseqIndv(i, businessInterruptionClass)
      createConseqIndv(i, hazardClass)
    }

    val scadaClass = ontModel.getOntClass(config.amlConfig.nsImp + "#SCADASystem")
    OntModelUtils.getOntResources(scadaClass.listInstances()) foreach { i =>
      createConseqIndv(i, businessInterruptionClass)
      createConseqIndv(i, hazardClass)
    }

    Some(ontModel)
  }

  def importSecOnts(config: Config, ontModel: OntModel): Option[OntModel] = {
    val ontSec = ResourceFactory.createResource(config.secOntConfig.ns)
    val ontIcsSec = ResourceFactory.createResource(config.icsSecOntConfig.ns)
    val ontAg = ResourceFactory.createResource(config.agOnt.ns)
    ontModel.listOntologies().toList.forEach { o =>
      o.addImport(ontSec)
      o.addImport(ontIcsSec)
      o.addImport(ontAg)
    }
    ontModel.getDocumentManager.getFileManager.setModelCaching(true)
    ontModel.loadImports()
    Some(ontModel)
  }

  private def addConceptEquivalenceAcrossNs(config: Config, ontModel: OntModel): Option[OntModel] = {
    /* Concept equivalence for protocols. */
    val icsProtocolClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Protocol")
    val amlProtocolClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Protocol")
    icsProtocolClass.addEquivalentClass(amlProtocolClass)
    val icsModbusClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Modbus")
    val amlModbusClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Modbus")
    icsModbusClass.addEquivalentClass(amlModbusClass)
    val icsOpcClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#OPC")
    val amlOpcClass = ontModel.getOntClass(config.amlConfig.nsImp + "#OPC")
    icsOpcClass.addEquivalentClass(amlOpcClass)
    val icsProfinetClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Profinet")
    val amlProfinetClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Profinet")
    icsProfinetClass.addEquivalentClass(amlProfinetClass)
    val icsProfisafeClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#PROFIsafe")
    val amlProfisafeClass = ontModel.getOntClass(config.amlConfig.nsImp + "#PROFIsafe")
    icsProfisafeClass.addEquivalentClass(amlProfisafeClass)
    val icsOpcUaClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#OPC-UA")
    val amlOpcUaClass = ontModel.getOntClass(config.amlConfig.nsImp + "#OPC-UA")
    icsOpcUaClass.addEquivalentClass(amlOpcUaClass)
    val icsHttpClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#HTTP")
    val amlHttpClass = ontModel.getOntClass(config.amlConfig.nsImp + "#HTTP")
    icsHttpClass.addEquivalentClass(amlHttpClass)
    val icsHttpsClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#HTTPS")
    val amlHttpsClass = ontModel.getOntClass(config.amlConfig.nsImp + "#HTTPS")
    icsHttpsClass.addEquivalentClass(amlHttpsClass)

    /* Concept equivalence for algorithm. */
    val icsBasic128Rsa15Class = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Basic128RSA15")
    val amlBasic128Rsa15Class = ontModel.getOntClass(config.amlConfig.nsImp + "#Basic128RSA15")
    icsBasic128Rsa15Class.addEquivalentClass(amlBasic128Rsa15Class)
    val icsBasic256Class = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Basic256")
    val amlBasic256Class = ontModel.getOntClass(config.amlConfig.nsImp + "#Basic256")
    icsBasic256Class.addEquivalentClass(amlBasic256Class)
    val icsBasic256Sha256Class = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Basic256SHA256")
    val amlBasic256Sha256Class = ontModel.getOntClass(config.amlConfig.nsImp + "#Basic256SHA256")
    icsBasic256Sha256Class.addEquivalentClass(amlBasic256Sha256Class)

    /* Concept equivalence for PPR. */
    val icsProcessClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Process")
    val amlProcessClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Process")
    icsProcessClass.addEquivalentClass(amlProcessClass)
    val icsProductClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Product")
    val amlProductClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Product")
    icsProductClass.addEquivalentClass(amlProductClass)
    val icsResourceClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#Resource")
    val amlResourceClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Resource")
    icsResourceClass.addEquivalentClass(amlResourceClass)

    /* Concept equivalence for software/program assets. */
    val secSoftwareClass = ontModel.getOntClass(config.secOntConfig.ns + "#Software")
    val amlProgramClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Program")
    secSoftwareClass.addEquivalentClass(amlProgramClass)

    Some(ontModel)
  }

  private def addAssetsSubclasses(config: Config, ontModel: OntModel): Option[OntModel] = {
    val icsSecOtComponentClass = ontModel.getOntClass(config.icsSecOntConfig.ns + "#OTComponent")
    val amlHostClass = ontModel.getOntClass(config.amlConfig.nsImp + "#Host")
    val amlNetworkDeviceClass = ontModel.getOntClass(config.amlConfig.nsImp + "#NetworkDevice")
    val amlSecurityDeviceClass = ontModel.getOntClass(config.amlConfig.nsImp + "#SecurityDevice")

    icsSecOtComponentClass.addSubClass(amlHostClass)
    icsSecOtComponentClass.addSubClass(amlNetworkDeviceClass)
    icsSecOtComponentClass.addSubClass(amlSecurityDeviceClass)

    Some(ontModel)
  }

  private def addAssetPhysicallyConnectedToAssetRoles(config: Config, ontModel: OntModel): Option[OntModel] = {
    QueryExecutor.construct(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX amlImp: <http://www.ipr.kit.edu/aml_importer#>
          |PREFIX amlOnt: <http://www.ipr.kit.edu/aml_ontology#>
          |PREFIX secOnt: <http://securityontology.com/secont#>
          |CONSTRUCT { ?asset1 secOnt:asset_physicallyConnectedTo_Asset ?asset2 }
          |       WHERE {
          |                 {
          |                     ?conn rdf:type ?wireType.
          |                     ?wireType rdfs:subClassOf* amlImp:Wire.
          |                 }
          |                 UNION
          |                 {
          |                     ?conn a amlImp:WirelessConnection.
          |                 }
          |
          |                 ?conn amlOnt:hasEI ?plug1, ?plug2.
          |                 FILTER ( str(?plug1) < str(?plug2) ).
          |
          |                 ?plug1 amlOnt:hasRefPartner ?link1.
          |                 ?plug2 amlOnt:hasRefPartner ?link2.
          |
          |                 ?link1 a amlImp:Link.
          |                 ?link2 a amlImp:Link.
          |
          |                 ?socket1 amlOnt:hasRefPartner ?link1.
          |                 ?socket2 amlOnt:hasRefPartner ?link2.
          |                 FILTER ( ?plug1 != ?socket1 ).
          |                 FILTER ( ?plug2 != ?socket2 ).
          |
          |                 ?portList1 amlOnt:hasEI ?socket1.
          |                 ?portList2 amlOnt:hasEI ?socket2.
          |
          |                 ?asset1 amlOnt:hasIE ?portList1.
          |                 ?asset2 amlOnt:hasIE ?portList2.
          |
          |                 ?zone1 amlOnt:hasIE+ ?asset1.
          |                 ?zone2 amlOnt:hasIE+ ?asset2.
          |
          |                 ?zone1 rdf:type ?zoneType.
          |                 ?zone2 rdf:type ?zoneType.
          |                 ?zoneType rdfs:subClassOf* amlImp:Zone.
          |
          |                 FILTER NOT EXISTS {
          |                    ?zone1 amlOnt:hasIE+ ?anyZone1.
          |                    ?anyZone1 amlOnt:hasIE+ ?asset1.
          |                    ?anyZone1 rdf:type ?anyZoneType.
          |                    ?anyZoneType rdfs:subClassOf* amlImp:Zone.
          |                 }
          |
          |                 FILTER NOT EXISTS {
          |                    ?zone2 amlOnt:hasIE+ ?anyZone2.
          |                    ?anyZone2 amlOnt:hasIE+ ?asset2.
          |                    ?anyZone2 rdf:type ?anyZoneType.
          |                    ?anyZoneType rdfs:subClassOf* amlImp:Zone.
          |                 }
          |
          |                 FILTER ( ?zone1 = ?zone2 ).
          |       }
        """.stripMargin,
      ontModel = ontModel
    ).map(m => ontModel.add(m).asInstanceOf[OntModel])
  }


  private def addAssetLogicallyConnectedToAssetRoles(config: Config, ontModel: OntModel): Option[OntModel] = {
    QueryExecutor.construct(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX amlImp: <http://www.ipr.kit.edu/aml_importer#>
          |PREFIX amlOnt: <http://www.ipr.kit.edu/aml_ontology#>
          |PREFIX secOnt: <http://securityontology.com/secont#>
          |CONSTRUCT { ?asset1 secOnt:asset_logicallyConnectedTo_Asset ?asset2 }
          |       WHERE {
          |                ?conn a amlImp:LogicalConnection.
          |
          |                 ?conn amlOnt:hasEI ?plug1, ?plug2.
          |                 FILTER ( str(?plug1) < str(?plug2) ).
          |
          |                 ?plug1 amlOnt:hasRefPartner ?link1.
          |                 ?plug2 amlOnt:hasRefPartner ?link2.
          |
          |                 ?link1 a amlImp:Link.
          |                 ?link2 a amlImp:Link.
          |
          |                 ?socket1 amlOnt:hasRefPartner ?link1.
          |                 ?socket2 amlOnt:hasRefPartner ?link2.
          |                 FILTER ( ?plug1 != ?socket1 ).
          |                 FILTER ( ?plug2 != ?socket2 ).
          |
          |                 ?portList1 amlOnt:hasEI ?socket1.
          |                 ?portList2 amlOnt:hasEI ?socket2.
          |
          |                 ?asset1 amlOnt:hasIE ?portList1.
          |                 ?asset2 amlOnt:hasIE ?portList2.
          |
          |                 ?zone1 amlOnt:hasIE+ ?asset1.
          |                 ?zone2 amlOnt:hasIE+ ?asset2.
          |
          |                 ?zone1 rdf:type ?zoneType.
          |                 ?zone2 rdf:type ?zoneType.
          |                 ?zoneType rdfs:subClassOf* amlImp:Zone.
          |
          |                 FILTER NOT EXISTS {
          |                    ?zone1 amlOnt:hasIE+ ?anyZone1.
          |                    ?anyZone1 amlOnt:hasIE+ ?asset1.
          |                    ?anyZone1 rdf:type ?anyZoneType.
          |                    ?anyZoneType rdfs:subClassOf* amlImp:Zone.
          |                 }
          |
          |                 FILTER NOT EXISTS {
          |                    ?zone2 amlOnt:hasIE+ ?anyZone2.
          |                    ?anyZone2 amlOnt:hasIE+ ?asset2.
          |                    ?anyZone2 rdf:type ?anyZoneType.
          |                    ?anyZoneType rdfs:subClassOf* amlImp:Zone.
          |                 }
          |
          |       }
        """.stripMargin,
      ontModel = ontModel
    ).map(m => ontModel.add(m).asInstanceOf[OntModel])
  }

}
