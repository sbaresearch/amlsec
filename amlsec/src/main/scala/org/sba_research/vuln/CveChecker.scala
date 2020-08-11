package org.sba_research.vuln

import org.apache.jena.rdf.model.Model
import org.sba_research.utils.{QueryExecutor, ResultSetBinding}

object CveChecker {


  def check(model: Model): ResultSetBinding = {
    val resBinding = QueryExecutor.query(
      s =
        """
          |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX owl: <http://www.w3.org/2002/07/owl#>
          |PREFIX aml: <http://www.ipr.kit.edu/aml_importer#>
          |SELECT DISTINCT ?part ?vendorName ?productName ?version
          |   WHERE {
          |      ?indvAml aml:hasCpePart ?part.
          |      ?indvAml aml:hasCpeVendor ?vendorName.
          |      ?indvAml aml:hasCpeProduct ?productName.
          |      ?indvAml aml:hasCpeVersion ?version.
          |   }
        """.stripMargin,
      model = model,
      resBinding = None
    )

    QueryExecutor.query(
      s =
        """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX sepsescpe: <http://w3id.org/sepses/vocab/ref/cpe#>
          |PREFIX sepsescve: <http://w3id.org/sepses/vocab/ref/cve#>
          |PREFIX sepsescvss: <http://w3id.org/sepses/vocab/ref/cvss#>
          |SELECT ?cve ?cveId ?cpe ?version ?other ?part ?product ?productName ?vendor ?vendorName ?cvss ?cvssBaseScore
          |   WHERE {
          |       ?cpe sepsescpe:version ?version ;
          |            sepsescpe:other ?other ;
          |            sepsescpe:part ?part ;
          |            sepsescpe:hasProduct ?product .
          |       ?cve sepsescve:id ?cveId ;
          |            sepsescve:hasCPE ?cpe .
          |       OPTIONAL {
          |            ?cve sepsescve:hasCVSS2BaseMetric ?cvss .
          |            ?cvss sepsescvss:baseScore ?cvssBaseScore .
          |       }
          |       ?product sepsescpe:productName ?productName ;
          |                sepsescpe:hasVendor ?vendor .
          |       ?vendor sepsescpe:vendorName ?vendorName .
          |   }
        """.stripMargin,
      service = """https://sepses.ifs.tuwien.ac.at/sparql""",
      resBinding = resBinding
    )

  }

}
