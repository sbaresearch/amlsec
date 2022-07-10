package org.sba_research.model

import com.typesafe.scalalogging.Logger
import org.apache.jena.ontology.{Individual, OntClass, OntModel, OntResource}
import org.apache.jena.rdf.model.{Model, ResourceFactory}
import org.apache.jena.util.iterator.ExtendedIterator
import org.apache.jena.vocabulary.OWL2
import org.sba_research.Config
import org.sba_research.utils.{OntModelUtils, QueryExecutor}

object AmlOntExtension {

  val logger: Logger = Logger(getClass)


  def augment(config: Config, ontModel: OntModel): Option[OntModel] =
    for {
      model <- addOntAxioms(config, ontModel)
      /* Add object properties 'asset_physicallyConnectedTo_Asset' */
      ontModelAssetConnectedToAssetRoles <- addAssetPhysicallyConnectedToAssetRoles(config, model).map(m => model.add(m).asInstanceOf[OntModel])
      /* Add object properties 'asset_logicallyConnectedTo_Asset' */
      ontModelAssetLogicallyConnectedToAssetRoles <- addAssetLogicallyConnectedToAssetRoles(config, ontModelAssetConnectedToAssetRoles).map(m => ontModel.add(m).asInstanceOf[OntModel])
    } yield ontModelAssetLogicallyConnectedToAssetRoles


  def addOntAxiomsSFCQuality(config: Config, ontModel: OntModel): Option[OntModel] =
    for {
      ontModelExecutedBy <- addExecutedByObjectProperty(config, ontModel) // Add `executedBy` (domain: `Step`, range: `Asset`)
      ontModelCorrespondsTo <- addCorrespondsToObjectProperty(config, ontModelExecutedBy) // Add `correspondsTo` (domain: `Step`)
      ontModelHasQualityCheck <- addHasQualityCheckObjectProperty(config, ontModelCorrespondsTo) // Add `hasQualityCheck` (domain: `Step`, range: `QualityCheck`)
      ontModelQualityCheckAppliesTo <- addQualityCheckAppliesTo(config, ontModelHasQualityCheck) // Add `qualityCheckAppliesTo` (inverse of: `hasQualityCheck`)
      ontModelHasQualityCondition <- addHasQualityCondition(config, ontModelQualityCheckAppliesTo) // Add `hasQualityCondition` (domain: `Step`, range: `QualityCondition`)
      ontModelQualityConditionAppliesTo <- addQualityConditionAppliesTo(config, ontModelHasQualityCondition) // Add `qualityConditionAppliesTo` (inverse of: `hasQualityCondition`)
      ontModelHasStepRef <- addHasStepRefObjectProperty(config, ontModelQualityConditionAppliesTo) // Add `hasStepRef` object property (range: `Step`)
      ontModelCorrespondsToInstantiation <- addCorrespondsToRelations(config, ontModelHasStepRef).map(m => ontModelHasStepRef.add(m).asInstanceOf[OntModel]) // Create `correspondsTo` relations
      ontModelExecutedByInstantiation <- addExecutedByRelations(config, ontModelCorrespondsToInstantiation).map(m => ontModelCorrespondsToInstantiation.add(m).asInstanceOf[OntModel]) // Create `executedBy` relations
      ontModelQualityConditionsInstantiation <- addQualityConditions(config, ontModelExecutedByInstantiation).map(m => ontModelExecutedByInstantiation.add(m).asInstanceOf[OntModel]) // Create quality condition individuals incl. relations
      ontModelQualityChecksInstantiation <- addQualityChecks(config, ontModelQualityConditionsInstantiation).map(m => ontModelQualityConditionsInstantiation.add(m).asInstanceOf[OntModel]) // Create quality check individuals incl. relations
    } yield ontModelQualityChecksInstantiation

  def addOntAxioms(config: Config, ontModel: OntModel): Option[OntModel] =
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
    } yield ontModelAssetsSubclasses

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
    classes
      .filterNot {
        x =>
          x == cls ||
            x.hasSubClass(cls) ||
            x.hasSuperClass(cls) ||
            excludedClasses.contains(x) ||
            x.getNameSpace != (config.amlConfig.nsImp + "#") // We also want to exclude classes that are not from the engineering ontology
      }.foreach(cls.addDisjointWith)
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

  def getAddAssetPhysicallyConnectedToAssetRolesQueryString(): String =
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
      |                 FILTER NOT EXISTS {
      |                    ?asset1 rdf:type ?physicalNetworkType.
      |                    ?physicalNetworkType rdfs:subClassOf* amlImp:PhysicalNetwork.
      |                 }
      |
      |                 FILTER NOT EXISTS {
      |                    ?asset2 rdf:type ?physicalNetworkType.
      |                    ?physicalNetworkType rdfs:subClassOf* amlImp:PhysicalNetwork.
      |                 }
      |
      |                 FILTER ( ?zone1 = ?zone2 ).
      |       }
        """.stripMargin

  def addAssetPhysicallyConnectedToAssetRoles(config: Config, model: Model, withInference: Boolean = true): Option[Model] = {
    QueryExecutor.construct(
      s = getAddAssetPhysicallyConnectedToAssetRolesQueryString(),
      model = model,
      reasonerUri = if (withInference) Some(config.reasonerUri) else None
    )
  }


  def getAddAssetLogicallyConnectedToAssetRolesQueryString(): String =
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
      |                 FILTER NOT EXISTS {
      |                    ?asset1 rdf:type ?logicalNetworkType.
      |                    ?logicalNetworkType rdfs:subClassOf* amlImp:LogicalNetwork.
      |                 }
      |
      |                 FILTER NOT EXISTS {
      |                    ?asset2 rdf:type ?logicalNetworkType.
      |                    ?logicalNetworkType rdfs:subClassOf* amlImp:LogicalNetwork.
      |                 }
      |
      |       }
        """.stripMargin

  def addAssetLogicallyConnectedToAssetRoles(config: Config, model: Model, withInference: Boolean = true): Option[Model] = {
    QueryExecutor.construct(
      s = getAddAssetLogicallyConnectedToAssetRolesQueryString(),
      model = model,
      reasonerUri = if (withInference) Some(config.reasonerUri) else None
    )
  }

  private def addExecutedByObjectProperty(config: Config, ontModel: OntModel): Option[OntModel] = {
    val executedBy = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#executedBy")
    val step = ontModel.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Step")
    val asset = ontModel.getOntClass(s"${config.secOntConfig.ns}#Asset")
    executedBy.addDomain(step)
    executedBy.addRange(asset)
    Some(ontModel)
  }

  private def addCorrespondsToObjectProperty(config: Config, ontModel: OntModel): Option[OntModel] = {
    val correspondsTo = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#correspondsTo")
    val step = ontModel.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Step")
    correspondsTo.addDomain(step)
    Some(ontModel)
  }

  private def addHasQualityCheckObjectProperty(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasQualityCheck = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasQualityCheck")
    val step = ontModel.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Step")
    val qualityCheck = ontModel.getOntClass(s"${config.qualOntConfig.ns}/QualityCheck")
    hasQualityCheck.addDomain(step)
    hasQualityCheck.addRange(qualityCheck)
    Some(ontModel)
  }

  private def addQualityCheckAppliesTo(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasQualityCheck = ontModel.getObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasQualityCheck")
    val qualityCheckAppliesTo = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualityCheckAppliesTo")
    hasQualityCheck.addInverseOf(qualityCheckAppliesTo)
    Some(ontModel)
  }

  private def addHasQualityCondition(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasQualityCondition = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasQualityCondition")
    val step = ontModel.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Step")
    val qualityCondition = ontModel.getOntClass(s"${config.qualOntConfig.ns}/QualityCondition")
    hasQualityCondition.addDomain(step)
    hasQualityCondition.addRange(qualityCondition)
    Some(ontModel)
  }

  private def addQualityConditionAppliesTo(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasQualityCondition = ontModel.getObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasQualityCondition")
    val qualityConditionAppliesTo = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualityConditionAppliesTo")
    hasQualityCondition.addInverseOf(qualityConditionAppliesTo)
    Some(ontModel)
  }

  private def addCorrespondsToRelations(config: Config, ontModel: OntModel, withInference: Boolean = true): Option[Model] = {

    def createCorrespondsToRelation(m: OntModel, opIndvsAndUuidList: List[(Individual, String)]): Option[Model] = {
      // Create VALUES part
      val valuesString = opIndvsAndUuidList.map(o =>
        s"""( <${o._1.getURI}> "${o._2}" )"""
      ).mkString("\n")
      logger.info(s"Adding `correspondsTo` relations for: $valuesString")
      // This query is only executed once to construct the `correspondsTo` relations
      QueryExecutor.construct(
        s =
          s"""
             |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             |PREFIX owl: <http://www.w3.org/2002/07/owl#>
             |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
             |PREFIX amlImp: <${config.amlConfig.nsImp}#>
             |PREFIX amlOnt: <${config.amlConfig.nsOnt}#>
             |PREFIX qual: <${config.qualOntConfig.ns}/>
             |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
             |CONSTRUCT {
             |       ?step sfc:correspondsTo ?qualOpIndv .
             |       ?ieOp sfc:hasStepRef ?step .
             |}
             |       WHERE {
             |                 ?step a ontoPlc:Step ;
             |                       ontoPlc:hasGlobalId ?globalId .
             |                 # Performance optimized
             |                 #?qualOpIndv rdf:type/rdfs:subClassOf* ?op .
             |                 #FILTER (?op IN (qual:ManufacturingOperation, qual:QualityControlMethod) ) .
             |                 VALUES ( ?ieOp ?uuid ) {
             |                         $valuesString
             |                 }
             |                 FILTER (?globalId = ?uuid) .
             |
             |                 ?ieOp a ?ieClass .
             |                 # Filter only direct sub-classes (e.g., amlImp:Gripping)
             |                 # Performance optimized
             |                 #?ieClass rdfs:subClassOf ?super .
             |                 #FILTER NOT EXISTS {
             |                  #     ?otherSub rdfs:subClassOf ?super.
             |                  #     ?ieClass rdfs:subClassOf ?otherSub .
             |                  #     FILTER (?otherSub != ?ieClass)
             |                  # }
             |                  ?ieClass rdfs:label ?ieOpLabel .
             |                  ?qualOpIndv qual:hasIdentifier ?qualOpId .
             |                  FILTER (?ieOpLabel = ?qualOpId ) .
             |       }
        """.stripMargin,
        model = m,
        reasonerUri = if (withInference) Some(config.reasonerUri) else None
      )
    }

    val queryResult = QueryExecutor.query(
      s =
        s"""
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |PREFIX owl: <http://www.w3.org/2002/07/owl#>
           |PREFIX amlImp: <${config.amlConfig.nsImp}#>
           |PREFIX amlOnt: <${config.amlConfig.nsOnt}#>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |SELECT ?manufacturingOperationOrQualityControl ?logicElementInterface
           |   WHERE {
           |      ?manufacturingOperationOrQualityControl rdf:type/rdfs:subClassOf* ?op ;
           |                                              amlOnt:hasEI ?logicElementInterface .
           |      FILTER (?op IN (amlImp:ManufacturingOperation, amlImp:QualityControlMethod) ) .
           |
           |      ?logicElementInterface a amlImp:LogicElementInterface .
           |   }
        """.stripMargin,
      model = ontModel,
      resBinding = None
    )

    val opIndvsAndUuidList = queryResult.values.flatMap { binding =>
      val opIndvOpt = queryResult.variables.find(_.getVarName == "manufacturingOperationOrQualityControl").map(binding.get).map(n => ontModel.getIndividual(n.getURI))
      val logicElIndvOpt = queryResult.variables.find(_.getVarName == "logicElementInterface").map(binding.get).map(n => ontModel.getIndividual(n.getURI))
      for {
        opIndv <- opIndvOpt
        logicElIndv <- logicElIndvOpt
      } yield {
        val hasRefUriProperty = ontModel.getDatatypeProperty(s"${config.amlConfig.nsImp}#hasRefURI")
        val refUriOpt = Option(logicElIndv.getPropertyValue(hasRefUriProperty))
        refUriOpt.flatMap { refUri =>
          val refUriString = refUri.asLiteral().toString
          refUriString.split("#").toList.lastOption.map(uuid => (opIndv, uuid))
        }
      }
    }.flatten

    createCorrespondsToRelation(ontModel, opIndvsAndUuidList)
  }

  private def addHasStepRefObjectProperty(config: Config, ontModel: OntModel): Option[OntModel] = {
    val hasStepRef = ontModel.createObjectProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasStepRef")
    val step = ontModel.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Step")
    hasStepRef.addRange(step)
    Some(ontModel)
  }

  private def addExecutedByRelations(config: Config, ontModel: OntModel, withInference: Boolean = true): Option[Model] =
    QueryExecutor.construct(
      s =
        s"""
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |PREFIX owl: <http://www.w3.org/2002/07/owl#>
           |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
           |PREFIX amlImp: <${config.amlConfig.nsImp}#>
           |PREFIX amlOnt: <${config.amlConfig.nsOnt}#>
           |PREFIX qual: <${config.qualOntConfig.ns}/>
           |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
           |PREFIX secOnt: <${config.secOntConfig.ns}#>
           |CONSTRUCT { ?step sfc:executedBy ?asset }
           |       WHERE {
           |       		?op amlOnt:hasEI ?interface1 ;
           |  				sfc:hasStepRef ?step .
           |			    ?interface1 a amlImp:ExecutedBy ;
           |                      amlOnt:hasRefPartner ?link .
           |  			  ?interface2 a amlImp:ExecutedBy ;
           |  				            amlOnt:hasRefPartner ?link .
           |
           |  			?link amlOnt:hasRefPartnerSideA ?interface1 ;
           |           		amlOnt:hasRefPartnerSideB ?interface2 ;
           |  				    a amlImp:Link .
           |
           |  			?asset amlOnt:hasEI ?interface2 ;
           |  					   a secOnt:Asset .
           |
           |        FILTER (?interface1 != ?interface2)
           |      }
        """.stripMargin,
      model = ontModel,
      reasonerUri = if (withInference) Some(config.reasonerUri) else None
    )

  private def addQualityConditions(config: Config, ontModel: OntModel, withInference: Boolean = true): Option[Model] =
    QueryExecutor.construct(
      s =
        s"""
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |PREFIX owl: <http://www.w3.org/2002/07/owl#>
           |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
           |PREFIX amlImp: <${config.amlConfig.nsImp}#>
           |PREFIX amlOnt: <${config.amlConfig.nsOnt}#>
           |PREFIX qual: <${config.qualOntConfig.ns}/>
           |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
           |PREFIX secOnt: <${config.secOntConfig.ns}#>
           |CONSTRUCT {
           |
           |      ?qualityCondition qual:preConditionProcess ?preOpQual ;
           |  		                  qual:postConditionProcess ?postOpQual ;
           | 			                  qual:preConditionQualityCharacteristic ?qual1 ;
           |  		                  qual:postConditionQualityCharacteristic ?qual2 ;
           |                        a qual:QualityCondition .
           |      ?preStep sfc:hasQualityCondition ?qualityCondition .
           |      ?postStep sfc:hasQualityCondition ?qualityCondition .
           |
           |}
           |     WHERE {
           |
           |         BIND("https://sba-research.org/sfctransformation#" AS ?sfcNs)
           |         BIND("http://www.qualityontology.org/" AS ?qualNs)
           |
           |         ?ieQualityCondition a amlImp:QualityCondition ;
           |  			                     amlOnt:hasIE ?preCondQual .
           |
           |  			 BIND(URI(REPLACE(STR(?ieQualityCondition), "^(.*?)#", CONCAT(?sfcNs, "qualitycondition_"))) AS ?qualityCondition)
           |
           |  			 ?preCondQual a amlImp:PreConditionQualityCharacteristic ;
           |                        amlOnt:hasIE ?preQual .
           |         ?ieQualityCondition amlOnt:hasIE ?postCondQual .
           |  			 ?postCondQual a amlImp:PostConditionQualityCharacteristic ;
           |                         amlOnt:hasIE ?postQual .
           |
           |  			  ?preQual rdf:type/rdfs:subClassOf* amlImp:QualityCharacteristic .
           |          ?postQual rdf:type/rdfs:subClassOf* amlImp:QualityCharacteristic .
           |
           |          ?preQual a ?ieClass1 .
           |          ?ieClass1 rdfs:label ?qual1Label .
           |
           |          FILTER(?ieClass1 != amlImp:QualityCharacteristic) .
           |
           | 				  ?postQual a ?ieClass2 .
           |          FILTER(?ieClass2 != amlImp:QualityCharacteristic) .
           |          ?ieClass2 rdfs:label ?qual2Label .
           |
           |          BIND(IRI(CONCAT(?qualNs, STR(?qual1Label))) AS ?qual1)
           |          BIND(IRI(CONCAT(?qualNs, STR(?qual2Label))) AS ?qual2)
           |
           |  				?ieQualityCondition amlOnt:hasEI ?postQualCondProcessEi1 ;
           |                              amlOnt:hasEI ?preQualCondProcessEi1 .
           |  				?postQualCondProcessEi1 a amlImp:PostQualityConditionProcessAppliesTo ;
           | 										              amlOnt:hasRefPartner ?link1 .
           |  				?postQualCondProcessEi2 a amlImp:PostQualityConditionProcessAppliesTo ;
           | 										              amlOnt:hasRefPartner ?link1 .
           |  				FILTER (?postQualCondProcessEi1 != ?postQualCondProcessEi2) .
           |
           |  				?postOp amlOnt:hasEI ?postQualCondProcessEi2 ;
           |          				a ?ieClass3 .
           |          FILTER (?ieClass3 != amlImp:ManufacturingOperation) .
           |  				?ieClass3 rdfs:label ?postOpLabel .
           |
           |          BIND(IRI(CONCAT(?qualNs, STR(?postOpLabel))) AS ?postOpQual)
           |
           |          ?preQualCondProcessEi1 a amlImp:PreQualityConditionProcessAppliesTo ;
           | 										             amlOnt:hasRefPartner ?link2 .
           |  				?preQualCondProcessEi2 a amlImp:PreQualityConditionProcessAppliesTo ;
           | 										             amlOnt:hasRefPartner ?link2 .
           |  				FILTER (?preQualCondProcessEi1 != ?preQualCondProcessEi2) .
           |
           |          ?preOp amlOnt:hasEI ?preQualCondProcessEi2 ;
           |          			  a ?ieClass4 .
           |          FILTER (?ieClass4 != amlImp:ManufacturingOperation) .
           |  				?ieClass4 rdfs:label ?preOpLabel .
           |
           |          BIND(IRI(CONCAT(?qualNs, STR(?preOpLabel))) AS ?preOpQual)
           |
           |          ?iePreProcess amlOnt:hasEI ?preQualCondProcessEi2 ;
           |                        sfc:hasStepRef ?preStep .
           |
           |          ?iePostProcess amlOnt:hasEI ?postQualCondProcessEi2 ;
           |                         sfc:hasStepRef ?postStep .
           |
           |      }
        """.stripMargin,
      model = ontModel,
      reasonerUri = if (withInference) Some(config.reasonerUri) else None
    )

  private def addQualityChecks(config: Config, ontModel: OntModel, withInference: Boolean = true): Option[Model] =
    QueryExecutor.construct(
      s =
        s"""
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |PREFIX owl: <http://www.w3.org/2002/07/owl#>
           |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
           |PREFIX amlImp: <${config.amlConfig.nsImp}#>
           |PREFIX amlOnt: <${config.amlConfig.nsOnt}#>
           |PREFIX qual: <${config.qualOntConfig.ns}/>
           |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
           |PREFIX secOnt: <${config.secOntConfig.ns}#>
           |CONSTRUCT {
           |
           |        ?qualityCheck qual:covers ?qual ;
           |                      qual:isCoveredBy ?qcMethod .
           |
           |        ?step1 sfc:hasQualityCheck ?qualityCheck .
           |        ?step2 sfc:hasQualityCheck ?qualityCheck .
           |
           |}
           |     WHERE {
           |
           |         ?ieQualityCheck a amlImp:QualityCheck ;
           |                				 amlOnt:hasEI ?preQualCheckEi1 ;
           |      							     amlOnt:hasEI ?postQualCheckEi1 .
           |
           |         BIND(URI(REPLACE(STR(?ieQualityCheck), "^(.*?)#", CONCAT("https://sba-research.org/sfctransformation#", "qualitycheck_"))) AS ?qualityCheck)
           |
           |         ?preQualCheckEi1 a amlImp:PreQualityCheckAppliesTo ;
           | 										              amlOnt:hasRefPartner ?link1 .
           |  			 ?preQualCheckEi2 a amlImp:PreQualityCheckAppliesTo ;
           | 										              amlOnt:hasRefPartner ?link1 .
           |  			 FILTER (?preQualCheckEi1 != ?preQualCheckEi2) .
           |
           |  			 ?iePreProcess rdf:type/rdfs:subClassOf* amlImp:ManufacturingOperation ;
           |  				             amlOnt:hasEI ?preQualCheckEi2 .
           |
           |  			 ?iePreProcess sfc:hasStepRef ?step1 .
           |
           |
           |         ?postQualCheckEi1 a amlImp:PostQualityCheckAppliesTo ;
           | 										       amlOnt:hasRefPartner ?link2 .
           |  			 ?postQualCheckEi2 a amlImp:PostQualityCheckAppliesTo ;
           | 										       amlOnt:hasRefPartner ?link2 .
           |  			 FILTER (?postQualCheckEi1 != ?postQualCheckEi2) .
           |
           |  			 ?iePostQual rdf:type/rdfs:subClassOf* amlImp:QualityControlMethod ;
           |  				           amlOnt:hasEI ?postQualCheckEi2 .
           |
           |  			  ?iePostQual sfc:hasStepRef ?step2 .
           |
           |  				?step2 sfc:correspondsTo ?qcMethod .
           |
           |
           |  				?ieQualityCheck amlOnt:hasIE ?coveredQuals .
           |  				?coveredQuals a amlImp:CoveredQualityCharacteristics ;
           |                        amlOnt:hasIE ?ieQual .
           |
           |
           |  				?ieQual rdf:type/rdfs:subClassOf* amlImp:QualityCharacteristic ;
           |                  a ?ieClass .
           |          ?ieClass rdfs:label ?qualLabel .
           |
           |  				FILTER(?ieClass != amlImp:QualityCharacteristic) .
           |
           |  				BIND(IRI(CONCAT("http://www.qualityontology.org/", STR(?qualLabel))) AS ?qual)
           |
           |      }
        """.stripMargin,
      model = ontModel,
      reasonerUri = if (withInference) Some(config.reasonerUri) else None
    )

}
