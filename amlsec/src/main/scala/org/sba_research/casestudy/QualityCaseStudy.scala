package org.sba_research.casestudy

import com.typesafe.scalalogging.Logger
import org.apache.jena.ontology.OntModel
import org.sba_research.Config
import org.sba_research.qopn._
import org.sba_research.utils.{OntModelUtils, QueryExecutor, ResultSetBinding}

import java.io.File
import java.lang.ProcessBuilder.Redirect
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source.fromFile
import scala.jdk.FutureConverters._
import scala.util.{Failure, Success, Using}

case class QualityCaseStudyResult(q2: String, q3: String, q4: List[String])

case class Q2Result(result: String)

case class Q3Result(result: String)

object QualityCaseStudy {

  val logger: Logger = Logger(getClass)

  def answerQuestions2To4(config: Config, ontModel: OntModel)(implicit executionContext: ExecutionContext): Future[QualityCaseStudyResult] = {

    def isReachable(firstStateLine: Option[String]) = firstStateLine match {
      case Some("NOSTATE") | None => false
      case _ => true
    }

    def q2(): Future[Q2Result] = {

      val queryResult = QueryExecutor.query(
        s =
          s"""
             |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             |PREFIX owl: <http://www.w3.org/2002/07/owl#>
             |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
             |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
             |PREFIX secOnt: <${config.secOntConfig.ns}#>
             |PREFIX qual: <${config.qualOntConfig.ns}/>
             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
             |SELECT ?qual ?step
             |   WHERE {
             |             ?step a ontoPlc:Step ;
             |                   sfc:executedBy ?asset ;
             |                   sfc:correspondsTo ?op .
             |             ?vulnerability secOnt:vulnerability_on_Asset ?asset .
             |             ?op qual:influences ?qual .
             |   }
        """.stripMargin,
        model = ontModel,
        resBinding = None
      )

      val qualPlacesList = getNamesOfPlacesForQualityCharacteristicsCompromisedFromQueryResult(queryResult)

      val qualPart = s"${qualPlacesList.map(x => s"($x").mkString("", " > 0) OR ", " > 0)")}"
      val statePredicate = s"( ( $qualPart ) AND (p_finish > 0) )"

      val (q2PathFilePath, q2StateFilePath) = getLoLAFilePaths(config.qopnConfig.lolaConfig.pathFilePath, config.qopnConfig.lolaConfig.stateFilePath, ".q2")

      val pb = getLoLAProcessBuilder(config.qopnConfig.lolaConfig.filePath, statePredicate, q2PathFilePath, q2StateFilePath)
      pb.redirectOutput(Redirect.appendTo(new File(config.qopnConfig.lolaConfig.outputFilePath)))
      pb.redirectError(Redirect.appendTo(new File(config.qopnConfig.lolaConfig.outputFilePath)))
      val process = pb.start()
      process.onExit().asScala.map { process =>
        if (process.exitValue() == 0) {
          // Read states files
          Using(fromFile(q2StateFilePath)) { source => source.getLines.toList } match {
            case Success(stateLines) =>
              if (isReachable(stateLines.headOption)) {
                logger.info(s"Q2: The predicate $statePredicate is reachable.")
                val result = stateLines.flatMap { line =>
                  line.split(" : ").headOption match {
                    case Some(s) if qualPlacesList.contains(s) => Some(s)
                    case None => throw new IllegalStateException(s"Failed to parse witness states for line $line.")
                    case _ => None
                  }
                }
                logger.info(s"Q2: Quality characteristics compromised through ${result.mkString(", ")}.")
                Q2Result(result.mkString(", "))
              } else {
                val res = s"The predicate $statePredicate is unreachable."
                logger.info("Q2: " + res)
                Q2Result(res)
              }
            case Failure(e) => throw e
          }
        } else throw new IllegalStateException(s"LoLA exited with ${process.exitValue()}.")
      }
    }


    def q3(): Future[Q3Result] = {

      val queryResult = QueryExecutor.query(
        s =
          s"""
             |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             |PREFIX owl: <http://www.w3.org/2002/07/owl#>
             |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
             |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
             |PREFIX secOnt: <${config.secOntConfig.ns}#>
             |PREFIX qual: <${config.qualOntConfig.ns}/>
             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
             |SELECT ?qual ?step
             |   WHERE {
             |             ?step a ontoPlc:Step ;
             |                   sfc:executedBy ?asset ;
             |                   sfc:correspondsTo ?op .
             |             ?op qual:influences ?qual .
             |             FILTER NOT EXISTS { ?vulnerability secOnt:vulnerability_on_Asset ?asset . }
             |   }
        """.stripMargin,
        model = ontModel,
        resBinding = None
      )

      val qualPlacesList = getNamesOfPlacesForQualityCharacteristicsCompromisedFromQueryResult(queryResult)

      val qualPart = s"${qualPlacesList.map(x => s"($x").mkString("", " > 0) OR ", " > 0)")}"
      val statePredicate = s"( ( $qualPart ) AND (p_finish > 0) )"

      val (q3PathFilePath, q3StateFilePath) = getLoLAFilePaths(config.qopnConfig.lolaConfig.pathFilePath, config.qopnConfig.lolaConfig.stateFilePath, ".q3")

      val pb = getLoLAProcessBuilder(config.qopnConfig.lolaConfig.filePath, statePredicate, q3PathFilePath, q3StateFilePath)
      pb.redirectOutput(Redirect.appendTo(new File(config.qopnConfig.lolaConfig.outputFilePath)))
      pb.redirectError(Redirect.appendTo(new File(config.qopnConfig.lolaConfig.outputFilePath)))
      val process = pb.start()
      process.onExit().asScala.map { process =>
        if (process.exitValue() == 0) {
          // Read states files
          Using(fromFile(q3StateFilePath)) { source => source.getLines.toList } match {
            case Success(stateLines) =>
              if (isReachable(stateLines.headOption)) {
                logger.info(s"Q3: The predicate $statePredicate is reachable.")
                val result = stateLines.flatMap { line =>
                  line.split(" : ").headOption match {
                    case Some(s) if qualPlacesList.contains(s) => Some(s)
                    case None => throw new IllegalStateException(s"Failed to parse witness states for line $line.")
                    case _ => None
                  }
                }
                logger.info(s"Q3: Quality characteristics compromised through ${result.mkString(", ")}.")
                Q3Result(result.mkString(", "))
              } else {
                val res = s"The predicate $statePredicate is unreachable."
                logger.info("Q3: " + res)
                Q3Result(res)
              }
            case Failure(e) => throw e
          }
        } else throw new IllegalStateException(s"LoLA exited with ${process.exitValue()}.")
      }
    }

    def q4(): Future[List[String]] = {

      val lolaQ4FilePath = config.qopnConfig.lolaConfig.filePath + ".q4"

      def executeReachabilityAnalysis(qopn: PetriNet, resourcesNeedToAttack: List[String] = List.empty): Future[List[String]] = {

        val statePredicate = s"(p_defects > 0)"

        val (q4PathFilePath, q4StateFilePath) = getLoLAFilePaths(config.qopnConfig.lolaConfig.pathFilePath, config.qopnConfig.lolaConfig.stateFilePath, ".q4")

        val pb = getLoLAProcessBuilder(lolaQ4FilePath, statePredicate, q4PathFilePath, q4StateFilePath)
        pb.redirectOutput(Redirect.appendTo(new File(config.qopnConfig.lolaConfig.outputFilePath)))
        pb.redirectError(Redirect.appendTo(new File(config.qopnConfig.lolaConfig.outputFilePath)))
        val process = pb.start()

        process.onExit().asScala.map { process: Process =>
          if (process.exitValue() == 0) {
            // Read states files
            val res = for {
              stateLines <- Using(fromFile(q4StateFilePath)) { source => source.getLines.toList }
              pathLines <- Using(fromFile(q4PathFilePath)) { source => source.getLines.toList }
            } yield {
              if (isReachable(stateLines.headOption)) {
                logger.info(s"Q4: The predicate $statePredicate is reachable.")
                pathLines.reverse.collectFirst {
                  case l if l.startsWith("t_defect_detected_resource_ok_quality_characteristic_compromised_") => l
                }.flatMap { elOfG5Dagger =>
                  // We have now T(i) in G^{\dagger}{_5}, next we need to retrieve place for the vulnerable resource
                  qopn.presetArcs.get(elOfG5Dagger) match {
                    case Some(v) =>
                      // First we have to find the place corresponding to the resource vulnerable complement
                      val pResourceVulnerableComplementOpt = v.values.find(_.source.id.endsWith("_vulnerable_complement")).map(_.source)
                      val pResourceVulnerableOpt = pResourceVulnerableComplementOpt
                        .flatMap(pVulnerableComplement => qopn.places.get(pVulnerableComplement.id.replace("_complement", "")))
                      for {
                        pResourceVulnerableComplement <- pResourceVulnerableComplementOpt
                        pResourceVulnerable <- pResourceVulnerableOpt
                      } yield {
                        // Next, we retrieve the index of the resource vulnerable complement place
                        val idxVulnerableComplement = qopn.marking.places.indexOf(pResourceVulnerableComplement)
                        // Then, we retrieve the index of the resource vulnerable place
                        val idxVulnerable = qopn.marking.places.indexOf(pResourceVulnerable)
                        // Then, remove the token from the list and add another for the resource vulnerable place
                        val tokens = Token(1) :: qopn.marking.tokens.patch(idxVulnerableComplement, Nil, 1).patch(idxVulnerable, Nil, 1)
                        // Finally, add the resource vulnerable place to the marking
                        val places = pResourceVulnerable :: qopn.marking.places.patch(idxVulnerableComplement, Nil, 1).patch(idxVulnerable, Nil, 1)
                        val newQopn = qopn.copy(
                          marking = Marking(places = places, tokens = tokens)
                        )
                        LoLAWriter(lolaQ4FilePath, newQopn) match {
                          case Right(_) =>
                            // Add the resource to the list
                            val resourceNeededToAttack = pResourceVulnerable.name.replace("_vulnerable", "")
                            val updatedListOfResourcesNeededToAttack = resourceNeededToAttack :: resourcesNeedToAttack
                            executeReachabilityAnalysis(newQopn, updatedListOfResourcesNeededToAttack)
                          case Left(e) =>
                            Future.failed(throw new IllegalStateException(e.message))
                        }
                      }
                    case None =>
                      throw new IllegalStateException(s"Could not retrieve place with ID $elOfG5Dagger from Petri net.")
                  }
                }
              } else {
                logger.info(s"Q4: The predicate $statePredicate is unreachable.")
                Some(Future.successful(resourcesNeedToAttack))
              }
            }
            Future.fromTry(res).flatMap {
              case Some(f) => f
              case None => Future.failed(throw new IllegalArgumentException("No future to unwrap when working on Q4."))
            }
          } else Future.failed(throw new IllegalArgumentException(s"LoLA exited with ${process.exitValue()}."))

        }.flatten
      }

      // As described in the paper, we consider a different scenario for Q4:
      // a) The vulnerability of the PLC controlling the ABB UT robot has been fixed.
      // b) CMM now also checks the dimensions of the blank (in addition to the edge conditions).

      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_240f13ab-3738-46f9-bbad-5c22249c12bf")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_a8d93601-8b81-4fe5-b1d1-ff6230045e31"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )

      /*

      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_1397a351-ab59-4f07-8bf6-d216328d9389")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_3ad94249-8852-4436-a31e-5e201f3922ec"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )



      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_6086691e-859c-4c1b-b4a9-7e1ddecfa339")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_1be359e0-b27d-49ea-b2c2-ee9cb48ea794"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )


      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_c3248e9a-b43f-4638-a1aa-0c2b92eb48bd")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_2ed89bcc-bea2-4810-8a90-6e4b9e125d75"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )


      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_849f1162-39e4-448f-ba0c-8932d8a8ced8")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_9705f450-8a79-4c54-aae6-646048000e35"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )


      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_bfa1d28c-739b-4652-ad3a-2bee37154326")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_a5013a6c-2af9-4566-b039-0ac5226e1e75"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )




      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_1f16e39f-8471-4b43-9a08-bb1ea6a41856")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_0c4d899d-f949-4fa3-bc16-44c7162bea63"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )


      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_ae77a5ca-e49e-4b3b-ab36-662fd3b8d807")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_9931e5c5-7e38-449b-aa4f-ac7d8b42844e"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )



      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_43b939c2-a04a-45f1-ae71-37dd7f55209b")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_2ba1d661-b243-4a71-b8dd-f5553a316acc"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )


      // Delete the vulnerability on the asset
      ontModel.removeAll(
        null,
        ontModel.getObjectProperty(s"${config.secOntConfig.ns}#vulnerability_on_Asset"),
        ontModel.getIndividual(s"${config.amlConfig.nsImp}#ie_SimaticS71516F_11_56086b42-8535-4c33-9fe4-7024fb9c926c")
      )

      // Add the `Dimensions` quality characteristic to the quality check
      ontModel.add(
        ontModel.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualitycheck_ie_QualityCheckTrimmingCMM_bf66beca-0418-4ebf-9a0a-fd5c100a79ba"),
        ontModel.getObjectProperty(s"${config.qualOntConfig.ns}/covers"),
        ontModel.getIndividual(s"${config.qualOntConfig.ns}/Dimensions")
      )


      */

      // Re-generate the QOPN
      val qopnGenerationResult = QOPNGenerator(config, ontModel)
      qopnGenerationResult match {
        case Right(r) =>
          LoLAWriter(lolaQ4FilePath, r.qopn) match {
            case Right(_) =>
              // Start reachability analysis
              executeReachabilityAnalysis(r.qopn)
            case Left(e) => throw new IllegalStateException(e.message)
          }
        case Left(e) =>
          throw new IllegalStateException(e.message)
      }

    }

    val f2 = q2()
    val f3 = q3()
    val f4 = q4()

    for {
      res2 <- f2
      res3 <- f3
      res4 <- f4
    } yield QualityCaseStudyResult(res2.result, res3.result, res4)

  }

  private def getNamesOfPlacesForQualityCharacteristicsCompromisedFromQueryResult(queryResult: ResultSetBinding): List[String] = queryResult.values.flatMap { binding =>
    for {
      step <- queryResult.variables.find(_.getVarName == "step").map(binding.get)
      qual <- queryResult.variables.find(_.getVarName == "qual").map(binding.get)
    } yield {
      val qcName = OntModelUtils.removeNamespace(qual.getURI)
      val stepId = OntModelUtils.removeNamespace(step.getURI)
      s"p_${qcName}_compromised_$stepId"
    }
  }

  private def getLoLAProcessBuilder(lolaFilePath: String, statePredicate: String, pathFilePath: String, stateFilePath: String) =
    new ProcessBuilder("lola", lolaFilePath, s"""--formula=EF $statePredicate""", s"--path=$pathFilePath", s"--state=$stateFilePath")

  private def getLoLAFilePaths(pathFilePath: String, stateFilePath: String, suffix: String): (String, String) =
    (pathFilePath + suffix, stateFilePath + suffix)

}
