package org.sba_research.ag

import java.io.File

import com.typesafe.scalalogging.Logger
import guru.nidi.graphviz.attribute.Rank.RankDir
import guru.nidi.graphviz.attribute.{Font, _}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.Factory._
import guru.nidi.graphviz.model.Node
import org.apache.jena.ontology.{Individual, OntClass, OntModel, OntTools}
import org.apache.jena.rdf.model.{InfModel, Model, Property, Resource}
import org.sba_research.Config
import org.sba_research.utils.{OntModelUtils, QueryExecutor}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

object AttackGraph {

  val logger = Logger(getClass)

  def generate(config: Config, model: Model, withInference: Boolean = true): Option[Model] = {
    QueryExecutor.construct(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
          |PREFIX amlImp: <http://www.ipr.kit.edu/aml_importer#>
          |PREFIX amlOnt: <http://www.ipr.kit.edu/aml_ontology#>
          |PREFIX secOnt: <http://securityontology.com/secont#>
          |PREFIX icsSecOnt: <http://securityontology.com/icssecont#>
          |PREFIX agOnt: <http://securityontology.com/agont#>
          |
          |CONSTRUCT {
          |     ?v1 a agOnt:Vertex ;
          |         agOnt:vertex_has_Edge ?e ;
          |         agOnt:vertex_has_Asset ?asset1 ;
          |         agOnt:vertex_has_Weight ?v1W .
          |     ?v2 a agOnt:Vertex ;
          |         agOnt:vertex_has_Asset ?asset2 ;
          |         agOnt:vertex_has_Weight ?v2W .
          |     ?e a agOnt:Edge ;
          |        agOnt:edge_has_Vulnerability ?vulnerability ;
          |        agOnt:edge_has_Vertex ?v2 .
          |}
          |WHERE {
          |
          |         {
          |
          |           SELECT ?v1 ?v2 ?asset1 ?asset2 ?e ?vulnerability (AVG(?vertexWeight1) AS ?v1W) (AVG(?vertexWeight2) AS ?v2W)
          |           WHERE {
          |
          |                   { ?asset1 secOnt:asset_physicallyConnectedTo_Asset ?asset2 . }
          |                   UNION
          |                   { ?asset1 secOnt:asset_logicallyConnectedTo_Asset ?asset2 . }
          |                   FILTER ( ?asset1 != ?asset2 ) .
          |                   { ?vulnerability secOnt:vulnerability_on_Asset ?asset2 . }
          |                   UNION
          |                   {
          |                      ?asset2 amlOnt:hasIE+ ?ie .
          |                      ?vulnerability secOnt:vulnerability_on_Asset ?ie .
          |                   }
          |                   FILTER NOT EXISTS {
          |                      ?asset1 rdf:type/rdfs:subClassOf* amlImp:NetworkDevice .
          |                   }
          |                   FILTER NOT EXISTS {
          |                      ?asset2 rdf:type/rdfs:subClassOf* amlImp:NetworkDevice .
          |                   }
          |                   OPTIONAL { ?asset1 secOnt:asset_impactedBy_Consequence ?consequence1 . }
          |                   OPTIONAL { ?asset2 secOnt:asset_impactedBy_Consequence ?consequence2 . }
          |                   BIND("http://securityontology.com/agont#" AS ?agNs)
          |                   BIND(URI(REPLACE(STR(?asset1), "^(.*?)#", CONCAT(?agNs, "vertex_"))) AS ?v1)
          |                   BIND(URI(REPLACE(STR(?asset2), "^(.*?)#", CONCAT(?agNs, "vertex_"))) AS ?v2)
          |                   BIND(URI(CONCAT(REPLACE(STR(?vulnerability), "^(.*?)#", CONCAT(?agNs, "edge_")), "_", REPLACE(STR(?asset1), "^(.*?)#", ""), "_", REPLACE(STR(?asset2), "^(.*?)#", ""))) AS ?e)
          |                   BIND(
          |                       EXISTS {
          |                         ?asset1 secOnt:asset_impactedBy_Consequence ?consequence1.
          |                       }
          |                       AS ?consequenceExists1 )
          |                     BIND(
          |                         IF(?consequenceExists1,
          |                           IF(
          |                             EXISTS {
          |                               ?consequence1 rdf:type ?hazardType.
          |                               ?hazardType rdfs:subClassOf* icsSecOnt:Hazard.
          |                             }
          |                             , "9.0"^^xsd:double,
          |                             IF(
          |                               EXISTS {
          |                                 ?consequence1 a secOnt:DataBreach.
          |                               }
          |                               , "5.0"^^xsd:double,
          |                               IF(
          |                                 EXISTS {
          |                                   ?consequence1 a secOnt:IPBreach.
          |                                 }
          |                                 , "6.0"^^xsd:double,
          |                                 IF(
          |                                   EXISTS {
          |                                     ?consequence1 a secOnt:BusinessInterruption.
          |                                   }
          |                                   , "7.0"^^xsd:double,
          |                                   IF(
          |                                     EXISTS {
          |                                       ?consequence1 a secOnt:RegulatoryNonCompliance.
          |                                     }
          |                                     , "7.5"^^xsd:double, "0.0"^^xsd:double
          |                                   )
          |                                 )
          |                               )
          |                             )
          |                           ), "0.0"^^xsd:double
          |                         ) AS ?vertexWeight1)
          |                     BIND(
          |                       EXISTS {
          |                         ?asset2 secOnt:asset_impactedBy_Consequence ?consequence2.
          |                       }
          |                       AS ?consequenceExists2 )
          |                     BIND(
          |                         IF(?consequenceExists2,
          |                           IF(
          |                             EXISTS {
          |                               ?consequence2 rdf:type ?hazardType.
          |                               ?hazardType rdfs:subClassOf* icsSecOnt:Hazard.
          |                             }
          |                             , "9.0"^^xsd:double,
          |                             IF(
          |                               EXISTS {
          |                                 ?consequence2 a secOnt:DataBreach.
          |                               }
          |                               , "5.0"^^xsd:double,
          |                               IF(
          |                                 EXISTS {
          |                                   ?consequence2 a secOnt:IPBreach.
          |                                 }
          |                                 , "6.0"^^xsd:double,
          |                                 IF(
          |                                   EXISTS {
          |                                     ?consequence2 a secOnt:BusinessInterruption.
          |                                   }
          |                                   , "7.0"^^xsd:double,
          |                                   IF(
          |                                     EXISTS {
          |                                       ?consequence2 a secOnt:RegulatoryNonCompliance.
          |                                     }
          |                                     , "7.5"^^xsd:double, "0.0"^^xsd:double
          |                                   )
          |                                 )
          |                               )
          |                             )
          |                           ), "0.0"^^xsd:double
          |                         ) AS ?vertexWeight2)
          |
          |                   }
          |                   GROUP BY ?v1 ?v2 ?asset1 ?asset2 ?e ?vulnerability
          |          }
          |
          |}
        """.stripMargin,
      model = model,
      reasonerUri = if (withInference) Some(config.reasonerUri) else None
    )
  }

  def plotShortestPathAg(config: Config, model: OntModel, a: Individual, b: Individual): Unit = {
    val vertexHasEdge = model.getObjectProperty(s"${config.agOnt.ns}#vertex_has_Edge")
    val edgeHasVertex = model.getObjectProperty(s"${config.agOnt.ns}#edge_has_Vertex")
    val vertexHasAssetProperty = model.getObjectProperty(s"${config.agOnt.ns}#vertex_has_Asset")
    val edgeHasVulnerabilityProperty = model.getObjectProperty(s"${config.agOnt.ns}#edge_has_Vulnerability")
    val props: Array[Property] = Array(vertexHasEdge, edgeHasVertex)
    val path = OntTools.findShortestPath(model, a, b, new OntTools.PredicatesFilter(props))
    val sources: List[Node] = path.listIterator().asScala.toList.sliding(2, 2).map { l =>
      val vert1 = model.getIndividual(l(0).getSubject.getURI)
      val e = model.getIndividual(l(1).getSubject.getURI)
      val vert2 = model.getIndividual(l(1).getObject.asNode().getURI)
      val asset1 = model.getIndividual(vert1.getPropertyValue(vertexHasAssetProperty).asResource().getURI)
      val asset2 = model.getIndividual(vert2.getPropertyValue(vertexHasAssetProperty).asResource().getURI)
      val vulnerability = model.getIndividual(e.getPropertyValue(edgeHasVulnerabilityProperty).asResource().getURI)
      node(getNodeLabel(asset1))
        .link(
          to(node(getNodeLabel(asset2)))
            .`with`(Style.SOLID, Label.of(getNodeLabel(vulnerability)), Color.BLACK)
        )
    }.toList
    val g = graph("shortest_path_ag")
      .directed
      .graphAttr
      .`with`(Rank.dir(RankDir.TOP_TO_BOTTOM))
      .`with`(sources.asJava)

    Graphviz.fromGraph(g).height(1000).render(Format.SVG).toFile(new File(config.agConfig.agShortestPath))
    ()
  }

  // Unpruned and without edge aggregation
  def plotFullAg(config: Config, model: OntModel): Unit = {
    val queryResult = QueryExecutor.query(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX amlImp: <http://www.ipr.kit.edu/aml_importer#>
          |PREFIX amlOnt: <http://www.ipr.kit.edu/aml_ontology#>
          |PREFIX secOnt: <http://securityontology.com/secont#>
          |PREFIX icsSecOnt: <http://securityontology.com/icssecont#>
          |PREFIX agOnt: <http://securityontology.com/agont#>
          |SELECT DISTINCT ?asset1 ?vulnerability ?asset2
          |      WHERE {
          |          ?v1 agOnt:vertex_has_Edge ?e.
          |          ?v1 agOnt:vertex_has_Asset ?asset1.
          |          ?e agOnt:edge_has_Vertex ?v2.
          |          ?v2 agOnt:vertex_has_Asset ?asset2.
          |          ?e agOnt:edge_has_Vulnerability ?vulnerability.
          |          ?v1 a agOnt:Vertex.
          |          ?v2 a agOnt:Vertex.
          |          ?e a agOnt:Edge.
          |          ?asset1 rdf:type ?assetType.
          |          ?asset2 rdf:type ?assetType.
          |          ?assetType rdfs:subClassOf* secOnt:Asset.
          |          ?vulnerability a secOnt:Vulnerability.
          |       }
        """.stripMargin,
      model = model,
      resBinding = None
    )

    val sources = queryResult.values.flatMap { binding =>
      val asset1Indv = queryResult.variables.find(_.getVarName == "asset1").map(binding.get).map(n => model.getIndividual(n.getURI))
      val asset2Indv = queryResult.variables.find(_.getVarName == "asset2").map(binding.get).map(n => model.getIndividual(n.getURI))
      val vulnIndv = queryResult.variables.find(_.getVarName == "vulnerability").map(binding.get).map(n => model.getIndividual(n.getURI))
      for {
        v1 <- asset1Indv
        v2 <- asset2Indv
        vuln <- vulnIndv
      } yield {
        val v1Lbl = getNodeLabel(v1)
        val v2Lbl = getNodeLabel(v2)
        val vulLbl = getNodeLabel(vuln)
        node(v1Lbl)
          .link(
            to(node(v2Lbl))
              .`with`(Style.SOLID, Label.of(vulLbl), Color.BLACK)
          )
      }
    }

    val g = graph("full_ag")
      .directed
      .graphAttr
      .`with`(Rank.dir(RankDir.LEFT_TO_RIGHT))
      .`with`(sources.asJava)


    Graphviz.fromGraph(g).height(1000).render(Format.SVG).toFile(new File(config.agConfig.fullPath))
    ()
  }

  def plotPrunedAg(config: Config, model: OntModel, withInference: Boolean = true): Unit = {

    val infModel = if (withInference) OntModelUtils.getInfModel(model, config.reasonerUri) else model

    val queryResult = QueryExecutor.query(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
          |PREFIX amlImp: <http://www.ipr.kit.edu/aml_importer#>
          |PREFIX amlOnt: <http://www.ipr.kit.edu/aml_ontology#>
          |PREFIX secOnt: <http://securityontology.com/secont#>
          |PREFIX icsSecOnt: <http://securityontology.com/icssecont#>
          |PREFIX agOnt: <http://securityontology.com/agont#>
          |SELECT DISTINCT ?asset1 ?vulnerability ?asset2 ?vulnScore ?v1Weight ?v2Weight
          |      WHERE {
          |          ?v1 agOnt:vertex_has_Edge ?e ;
          |              agOnt:vertex_has_Asset ?asset1 .
          |          ?e agOnt:edge_has_Vertex ?v2 ;
          |             agOnt:edge_has_Vulnerability ?vulnerability .
          |          ?v2 agOnt:vertex_has_Asset ?asset2 .
          |          ?v1 a agOnt:Vertex .
          |          ?v2 a agOnt:Vertex .
          |          ?e a agOnt:Edge .
          |          ?asset1 rdf:type ?assetType .
          |          ?asset2 rdf:type ?assetType .
          |          ?assetType rdfs:subClassOf* secOnt:Asset .
          |          ?vulnerability a secOnt:Vulnerability ;
          |                         secOnt:vulnerability_has_SeverityValue ?vulnScore .
          |          ?v1 agOnt:vertex_has_Weight ?v1Weight .
          |          ?v2 agOnt:vertex_has_Weight ?v2Weight .
          |
          |          FILTER NOT EXISTS {
          |             { ?asset1 a amlImp:Robot . }
          |             UNION
          |             { ?asset1 a amlImp:PLC . }
          |             UNION
          |             { ?asset1 a amlImp:SIS . }
          |             UNION
          |             { ?asset1 a amlImp:WirelessHARTSensor . }
          |          }
          |
          |          FILTER NOT EXISTS {
          |             { ?asset2 a amlImp:PLC . }
          |             UNION
          |             { ?asset2 a amlImp:SIS . }
          |             UNION
          |             { ?asset2 a amlImp:WirelessHARTSensor . }
          |             FILTER( ?v2Weight < "9.0"^^xsd:double )
          |          }
          |
          |          ?v1 (agOnt:vertex_has_Edge/agOnt:edge_has_Vertex)+ ?vOfInterest .
          |          ?vOfInterest agOnt:vertex_has_Asset ?assetOfInterest ;
          |                       agOnt:vertex_has_Weight ?vOfInterestWeight .
          |          FILTER( ?vOfInterestWeight >= "9.0"^^xsd:double )
          |          { ?assetOfInterest a amlImp:PLC . }
          |          UNION
          |          { ?assetOfInterest a amlImp:SIS . }
          |          UNION
          |          { ?assetOfInterest a amlImp:WirelessHARTSensor . }
          |       }
        """.stripMargin,
      model = infModel,
      resBinding = None
    )

    val zoneHierarchy = List(
      model.getOntClass(s"${config.amlConfig.nsImp}#EnterpriseZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#EnterpriseDmzZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#BusinessZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#DmzZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#OperationsSupportZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#SCADAZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#ControlZone"),
      model.getOntClass(s"${config.amlConfig.nsImp}#AutomationCellZone"),
    )

    logger.info("Pruning AG (edges with highest severity)...")

    val sources = queryResult.values.flatMap { binding =>
      val asset1Indv = queryResult.variables.find(_.getVarName == "asset1").map(binding.get).map(n => model.getIndividual(n.getURI))
      val asset2Indv = queryResult.variables.find(_.getVarName == "asset2").map(binding.get).map(n => model.getIndividual(n.getURI))
      val vulnIndv = queryResult.variables.find(_.getVarName == "vulnerability").map(binding.get).map(n => model.getIndividual(n.getURI))
      val vulnScore = queryResult.variables.find(_.getVarName == "vulnScore").map(binding.get).map(n => n.getLiteralValue.asInstanceOf[Double])
      for {
        v1 <- asset1Indv
        v2 <- asset2Indv
        vuln <- vulnIndv
        score <- vulnScore
      } yield (v1, v2, vuln, score)
    }
      // Edge pruning based on severity score
      // Group by asset1 and asset2 individual and get maximum vulnerability severity score
      .groupBy(x => (x._1, x._2)).values.map { n => n.maxBy(_._4) }
      // Edge pruning based on asset connections
      // Only keep edges that connect asset1 and asset2 if they are in the same zone layer or if asset2 is in a zone below asset1
      .flatMap { case (v1, v2, vuln, score) =>

        val zoneOfAsset1 = getZoneFromAsset(config, model, infModel, v1)
        val zoneOfAsset2 = getZoneFromAsset(config, model, infModel, v2)

        logger.info(s"$v1 (zone: $zoneOfAsset1), $v2 (zone: $zoneOfAsset2), $vuln, $score")

        // Check whether asset 1 is in higher or same layer than asset 2
        if (zoneHierarchy.indexOf(zoneOfAsset1) <= zoneHierarchy.indexOf(zoneOfAsset2)) {
          val v1Lbl = getNodeLabel(v1)
          val v2Lbl = getNodeLabel(v2)
          val vulLbl = getNodeLabel(vuln)
          Some(
            node(v1Lbl).`with`(Font.name("Times New Roman"))
              .link(
                to(node(v2Lbl).`with`(Font.name("Times New Roman")))
                  .`with`(Style.SOLID, Label.of(vulLbl), Color.BLACK, Font.name("Times New Roman"))
              )
          )
        } else None // Asset 1 is in a 'lower' zone than asset 2 (--> do not create an edge)

      }.toList

    val g = graph("pruned_ag")
      .directed
      .graphAttr
      .`with`(Rank.dir(RankDir.TOP_TO_BOTTOM), Rank.sep(0), GraphAttr.sizeMax(27.5, 2.5), Attributes.attr("ratio", "compress"))
      .`with`(sources.asJava)

    Graphviz.fromGraph(g).height(1000).render(Format.SVG).toFile(new File(config.agConfig.prunedPath))
    ()
  }


  // TODO
  def plotShortestHighestWeightedPathAg = throw new NotImplementedError()

  private def getNodeLabel(indv: Individual): String =
    if (indv.getLabel(null) != null) indv.getLabel(null) else OntModelUtils.removeNamespace(indv.getURI)

  def getZoneFromAsset(config: Config, ontModel: OntModel, infModel: InfModel, asset: Individual): OntClass = {

    val hasIEInvObjectProperty = infModel.getProperty(s"${config.amlConfig.nsOnt}#hasIEInv")
    val zoneClass = ontModel.getOntClass(s"${config.amlConfig.nsImp}#Zone")
    val zoneClasses = OntModelUtils.getAllSubclasses(zoneClass)
      /* We have to filter out the very same Zone class, because when we materialize the inferences beforehand, it will have a subclass with itself. */
      .filterNot(_ == zoneClass)
    val assetRes = infModel.getResource(asset.getURI)

    @tailrec
    def get(res: Resource): OntClass = {
      /* Get resource from hasIEInv object property. */
      val ieInvResource = res.getPropertyResourceValue(hasIEInvObjectProperty)
      /* Retrieve individual from ontology model. */
      val ieInvIndv = ontModel.getIndividual(ieInvResource.getURI)
      /* Retrieve all classes of individual. */
      val ieInvIndvClasses: List[OntClass] = ieInvIndv.listOntClasses(false).asScala.toList
      /* Get only the classes of the individual that are actually a zone. */
      /* This is necessary if we materialize the inferences beforehand, since there will be then multiple non-relevant classes (e.g., owl:Thing, rdfs:Resource). */
      val ieInvIndvZoneClasses = ieInvIndvClasses
        /* Filter out all non-zone classes. */
        .filter(zoneClasses.contains(_))
      val cls =
      /* If individual has more than one zone class, we know that we have materialized the inferences before. */
        if (ieInvIndvZoneClasses.size > 1)
        /* Find the one zone that has no subclasses. */
        ieInvIndvZoneClasses.find { clzz =>
          /* First filter out the current zone class. */
          ieInvIndvZoneClasses.filterNot(_ == clzz)
            /* Then, check if there exists a zone class that has the current zone class as a subclass. */
            .exists(_.hasSubClass(clzz))
        } else /* No prior materialization of inferences; thus, we have only one zone class. Take the head.  */
        ieInvIndvZoneClasses.headOption
      cls match {
        case Some(ontclzz: OntClass) => ontclzz // Zone found, return
        case None =>
          /* Get resource from inference model to make recursive call. */
          val ieInvRes = infModel.getResource(ieInvIndv.getURI)
          get(ieInvRes)
      }
    }

    get(assetRes)
  }

}
