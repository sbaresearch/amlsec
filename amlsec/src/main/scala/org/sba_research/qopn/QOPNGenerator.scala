package org.sba_research.qopn

import com.typesafe.scalalogging.Logger
import org.apache.jena.ontology.{Individual, OntModel}
import org.apache.jena.rdf.model.{InfModel, Property}
import org.sba_research.Config
import org.sba_research.utils.{OntModelUtils, QueryExecutor}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters._

case class QOPNGenerationError(message: String)

case class QOPNGenerationResult(qopn: PetriNet)

case class ProcessFlowResult(
                              qopn: PetriNet,
                              firstResourceStartsTransition: Option[Transition] = None,
                              lastTransitionToNextStepPlace: Option[Place] = None,
                              lastTransitionToNextStepPlace2: Option[Place] = None, // Used for second branch of divergence elements
                              transitionToFinish: Option[Transition] = None,
                              placeOrJoinBeforeAuxTransition: Option[Place] = None,
                              placeOrJoinBeforeAuxTransition2: Option[Place] = None, // Used for second branch of divergence elements
                              // After we have processed the first branch of a divergence,
                              // we store a copy of lastTransitionToNextStepPlace and placeOrJoinBeforeAuxTransition in these fields
                              copyOfLastTransitionToNextStepPlaceForFirstBranch: Option[Place] = None,
                              copyOfPlaceOrJoinBeforeAuxTransition: Option[Place] = None
                            )

// This case class models the dependencies between quality characteristics
case class QualityCondition(
                             preProcessStep: Individual,
                             postProcessStep: Individual,
                             preQualityCharacteristics: List[Individual],
                             postQualityCharacteristics: List[Individual],
                             preSfcStep: Individual,
                           )

// This case class models the mapping between quality control methods and the quality characteristics they check
case class QualityCheck(
                         covers: List[Individual], // The quality characteristics that are checked by the QC
                         isCoveredBy: Individual, // The QC method
                         preSfcStep: Option[Individual] // The (previous) SFC step that influences the quality characteristic to be checked, can be None if it corresponds to current manufacturing operation
                       )

object QOPNGenerator {

  val logger: Logger = Logger(getClass)

  def apply(config: Config, ontModel: OntModel): Either[QOPNGenerationError, QOPNGenerationResult] = {

    val infModel = OntModelUtils.getInfModel(ontModel, config.reasonerUri)

    // 1. Get Map with 'ie_Asset' -> true/false vulnerable
    getVulnerabilityInfoForAssets(config, ontModel) match {
      case Right(vulnerableAssetsMap) =>

        // 2. Get individual of the initial step
        getInitialStepIndividual(config, ontModel) match {
          case Right(indvInitialStep) =>
            // 3. Initialize Petri net
            val pn = PetriNet(id = "pn-1", name = "qopn_case_study_generated",
              marking = Marking(List.empty, List.empty),
              places = SortedMap.empty,
              transitions = SortedMap.empty,
              arcs = SortedMap.empty
            )
            // 4. Process flow till end
            processFlow(
              config = config, ontModel = ontModel, infModel = infModel, vulnerableAssetsMap = vulnerableAssetsMap,
              indvStep = indvInitialStep, result = ProcessFlowResult(qopn = pn)
            ) match {
              case Right(result) =>
                val qopn = result.qopn
                // Job Order Status:
                // Place for initial step
                val pJobOrderStatus = Place(id = "p_job_order_status", name = "Job_Order_Status")
                // Arc for initial step and first transition
                val aJobOrderStatusToFirst = result.firstResourceStartsTransition.map(trans =>
                  (pJobOrderStatus.id, trans.id) -> Arc(s"a_job_order_status_to_first_resource_starts", pJobOrderStatus, trans, 1)
                )

                // Finish:
                // Place for finish
                val pFinish = Place(id = "p_finish", name = "Finish")
                val aLastTransitionToFinish = result.transitionToFinish.map(trans =>
                  (trans.id, pFinish.id) -> Arc(s"a_last_transition_to_finish", trans, pFinish, 1)
                )

                val arcsToAdd = List(aJobOrderStatusToFirst, aLastTransitionToFinish).flatten

                // 5. Construct final Petri net
                val retPn = PetriNet.updateUsing(
                  petriNet = qopn,
                  places = Some(
                    qopn.places.+(
                      pJobOrderStatus.id -> pJobOrderStatus,
                      pFinish.id -> pFinish
                    )
                  ),
                  marking = Some(Marking(pJobOrderStatus :: qopn.marking.places, Token(1L) :: qopn.marking.tokens)),
                  arcs = Some(qopn.arcs.++(arcsToAdd))
                )

                Right(QOPNGenerationResult(retPn))

              case Left(processFlowError) => Left(processFlowError)
            }
          case Left(initialStepError) => Left(initialStepError)
        }

      case Left(vulnAssetsError) => Left(vulnAssetsError)
    }

  }


  private def getInitialStepIndividual(config: Config, ontModel: OntModel): Either[QOPNGenerationError, Individual] = {
    val queryResult = QueryExecutor.query(
      s =
        s"""
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |PREFIX owl: <http://www.w3.org/2002/07/owl#>
           |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |SELECT DISTINCT ?initialStep
           |   WHERE {
           |      ?initialStep ontoPlc:isInitialStep "true"^^xsd:boolean.
           |   }
        """.stripMargin,
      model = ontModel,
      resBinding = None
    )
    val indvInitialStep = queryResult.values.map { binding =>
      queryResult.variables.find(_.getVarName == "initialStep").map(binding.get)
    }.headOption.flatten
    indvInitialStep.map { optIndv => Right(ontModel.getIndividual(optIndv.getURI)) }
      .getOrElse(Left(QOPNGenerationError("Could not retrieve initial step from ontology model.")))
  }

  private def getVulnerabilityInfoForAssets(config: Config, ontModel: OntModel): Either[QOPNGenerationError, Map[String, Boolean]] = {
    val queryResult = QueryExecutor.query(
      s =
        s"""
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
           |PREFIX owl: <http://www.w3.org/2002/07/owl#>
           |PREFIX ontoPlc: <${config.sfcConfig.ontoPlcConfig.ns}#>
           |PREFIX sfc: <${config.sfcConfig.sfcTransformationOntConfig.ns}#>
           |PREFIX secOnt: <${config.secOntConfig.ns}#>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |SELECT ?asset ?vulnerable
           |   WHERE {
           |             ?step a ontoPlc:Step ;
           |                   sfc:executedBy ?asset .
           |             BIND(EXISTS{ ?vulnerability secOnt:vulnerability_on_Asset ?asset } AS ?vulnerable) .
           |   }
        """.stripMargin,
      model = ontModel,
      resBinding = None
    )
    val m = queryResult.values.flatMap { binding =>
      for {
        asset <- queryResult.variables.find(_.getVarName == "asset").map(binding.get)
        vulnerable <- queryResult.variables.find(_.getVarName == "vulnerable").map(binding.get)
      } yield {
        (OntModelUtils.removeNamespace(asset.getURI), vulnerable.getLiteralValue.asInstanceOf[Boolean])
      }
    }.toMap
    Right(m)
  }

  @tailrec
  private def processFlow(
                           config: Config, ontModel: OntModel,
                           infModel: InfModel, vulnerableAssetsMap: Map[String, Boolean],
                           indvStep: Individual, remainingIndvFromDivergence: Option[Individual] = None,
                           skipRetrievalOfNextStep: Boolean = false, result: ProcessFlowResult,
                           processSecondBranchOfDivergence: Boolean = false
                         ): Either[QOPNGenerationError, ProcessFlowResult] = {

    logger.info(s"Processing flow ${indvStep}.")

    val petriNet = result.qopn

    val nextSteps = if (skipRetrievalOfNextStep) Right((List(indvStep), false))
    else getNextSteps(config, ontModel, indvStep, remainingIndvFromDivergence)
    nextSteps match {
      case Right((nextIndividual :: Nil, isAfterConvergence)) =>

        val stepId = OntModelUtils.removeNamespace(nextIndividual.getURI)
        val notVulnerableResourceDetectsDefectIfQualityCharacteristicIsCompromised = true

        // Check if step is manufacturing operation or quality check
        getCorrespondingOperation(config, ontModel, nextIndividual) match {
          case Right(operation) =>
            val processClass = ontModel.getOntClass(s"${config.qualOntConfig.ns}/ManufacturingOperation")
            val qcMethodClass = ontModel.getOntClass(s"${config.qualOntConfig.ns}/QualityControlMethod")
            if (operation.hasOntClass(processClass)) {
              processManufacturingOperation(
                config, ontModel, infModel, nextIndividual, result, petriNet, stepId, vulnerableAssetsMap,
                processSecondBranchOfDivergence, isAfterConvergence
              ) match {
                case Right(newProcessFlowResult) =>
                  processFlow(
                    config, ontModel, infModel, vulnerableAssetsMap, nextIndividual, remainingIndvFromDivergence, skipRetrievalOfNextStep = false,
                    result = newProcessFlowResult
                  )
                case l@Left(_) => l
              }
            } else if (operation.hasOntClass(qcMethodClass)) {
              processQualityControlOperation(
                config, ontModel, infModel, nextIndividual, result, petriNet, stepId, vulnerableAssetsMap,
                notVulnerableResourceDetectsDefectIfQualityCharacteristicIsCompromised,
                processSecondBranchOfDivergence, isAfterConvergence
              ) match {
                case Right(newProcessFlowResult) =>
                  processFlow(
                    config, ontModel, infModel, vulnerableAssetsMap, nextIndividual, remainingIndvFromDivergence, skipRetrievalOfNextStep = false,
                    result = newProcessFlowResult
                  )
                case l@Left(_) => l
              }
            } else Left(QOPNGenerationError(s"Step ${nextIndividual} neither corresponds to a ${processClass} nor ${qcMethodClass}."))
          case Left(error) => Left(error)
        }

      case Right((x :: xs, isAfterConvergence)) =>
        logger.info(s"Processing divergence step, next connected steps are: $x and ${xs.head}.")
        logger.info(s"We directly create a second 'transition to next step' place for the divergence.")
        val stepId = OntModelUtils.removeNamespace(indvStep.getURI)
        petriNet.places.get(s"p_transition_$stepId") match {
          case Some(firstTransitionToNextStepPlace) =>
            petriNet.presetArcs.get(firstTransitionToNextStepPlace.id) match {
              case Some(presetArcsFirstTransitionToNextStepPlace) =>
                val sourceNodes = presetArcsFirstTransitionToNextStepPlace.toList.map(_._2.source)

                if (sourceNodes.isEmpty)
                  Left(QOPNGenerationError(s"Could not retrieve source nodes from preset arcs from ${firstTransitionToNextStepPlace.id}."))
                else {
                  val pTransitionToNextStep2 = Place(id = s"p_transition_2_$stepId", name = s"transition_2_$stepId")

                  // Now we need to check if the current step is a manufacturing operation or a QC operation
                  // If it is a manufacturing operation, we need to create an additional OR join place
                  val pOrJoin2AndArcs = getCorrespondingOperation(config, ontModel, indvStep) match {
                    case Right(operation) =>
                      val processClass = ontModel.getOntClass(s"${config.qualOntConfig.ns}/ManufacturingOperation")
                      if (operation.hasOntClass(processClass)) {
                        petriNet.places.get(s"p_or_join_1_$stepId") match {
                          case Some(pOrJoin1) =>
                            // We want to set the same preset arcs of pOrJoin1 for pOrJoin2
                            petriNet.presetArcs.get(pOrJoin1.id) match {
                              case Some(presetArcsOrJoin1) =>
                                val sourceNodes = presetArcsOrJoin1.map(_._2.source)
                                val pOrJoin2 = Place(id = s"p_or_join_2_divergence_$stepId", name = s"or_join_2_divergence_$stepId")
                                // Create the arcs where pOrJoin2 is the target
                                val arcsToAddForOrJoin2 = sourceNodes.map { n =>
                                  (n.id, pOrJoin2.id) -> Arc(id = s"${n.id}_to_${pOrJoin2.id}", n, pOrJoin2, 1)
                                }
                                Right(Some((pOrJoin2, arcsToAddForOrJoin2)))
                              case None => Left(QOPNGenerationError(s"Could not retrieve preset arcs from place ${pOrJoin1.id}."))
                            }
                          case None => Left(QOPNGenerationError(s"Could not retrieve OR join 1 place from Petri net for step $stepId."))
                        }
                      }
                      else Right(None) // Seems to be a quality control operation, no manufacturing operation
                    case Left(error) => Left(error)
                  }

                  pOrJoin2AndArcs match {
                    case Right(opts) =>

                      val placesToAdd = List(opts.map(x => x._1.id -> x._1)).flatten ::: List(
                        pTransitionToNextStep2.id -> pTransitionToNextStep2
                      )

                      val arcsToAdd = opts.map(_._2).toList.flatten ::: sourceNodes.map { n =>
                        (n.id, pTransitionToNextStep2.id) ->
                          Arc(id = s"${n.id}_to_${pTransitionToNextStep2.id}", n, pTransitionToNextStep2, 1)
                      }

                      // Add the new place and the arc to the Petri net
                      val res = result.copy(
                        qopn = PetriNet.updateUsing(
                          petriNet,
                          places = Some(
                            petriNet.places
                              .++(placesToAdd)
                          ),
                          arcs = Some(
                            petriNet.arcs
                              .++(arcsToAdd)
                          )
                        ),
                        lastTransitionToNextStepPlace2 = Some(pTransitionToNextStep2), // Here, we set the new place to ensure that the step is connected to the next
                        placeOrJoinBeforeAuxTransition2 = opts.map(_._1)
                      )
                      processFlow(
                        config, ontModel, infModel, vulnerableAssetsMap, x,
                        remainingIndvFromDivergence = xs.headOption, // Here, we set the other path that will be processed when we reach a convergence step
                        skipRetrievalOfNextStep = true,
                        result = res
                      )

                    case Left(error) => Left(error)
                  }
                }
              case None => Left(QOPNGenerationError(s"Could not retrieve preset arcs from place ${firstTransitionToNextStepPlace.id}."))
            }
          case None => Left(QOPNGenerationError(s"Could not retrieve 'transition to next step' place from Petri net for step $stepId."))
        }

      case Right((Nil, isAfterConvergence)) =>

        remainingIndvFromDivergence match {
          case Some(indv) =>
            logger.info("Next step is none but second branch of divergence has not been processed yet, so process remaining steps until converge...")
            processFlow(
              config, ontModel, infModel, vulnerableAssetsMap, indv, None,
              skipRetrievalOfNextStep = true,
              result = result.copy(
                copyOfLastTransitionToNextStepPlaceForFirstBranch = result.lastTransitionToNextStepPlace,
                copyOfPlaceOrJoinBeforeAuxTransition = result.placeOrJoinBeforeAuxTransition
              ),
              processSecondBranchOfDivergence = true
            )

          case None =>
            val auxId = java.util.UUID.randomUUID.toString.replace("-", "_")
            val tAuxiliary = Transition(id = s"t_auxiliary_$auxId", name = s"auxiliary_$auxId")

            val arcFromPlaceOrJoinToAuxTransition = result.placeOrJoinBeforeAuxTransition.map { p =>
              (p.id, tAuxiliary.id) -> Arc(s"a_or_join_1_to_auxiliary_$auxId", p, tAuxiliary, 1)
            }

            val arcFromPlaceOrJoin2ToAuxTransition = result.placeOrJoinBeforeAuxTransition2.map { p =>
              (p.id, tAuxiliary.id) -> Arc(s"a_or_join_2_to_auxiliary_$auxId", p, tAuxiliary, 1)
            }

            val arcFromTransitionToNextStepPlaceToAuxTransition = result.lastTransitionToNextStepPlace.map { p =>
              (p.id, tAuxiliary.id) -> Arc(s"a_transition_to_next_step_to_auxiliary_$auxId", p, tAuxiliary, 1)
            }

            val arcsToAdd = List(
              arcFromPlaceOrJoinToAuxTransition,
              arcFromPlaceOrJoin2ToAuxTransition,
              arcFromTransitionToNextStepPlaceToAuxTransition
            ).flatten

            val pn = PetriNet.updateUsing(
              petriNet = result.qopn,
              transitions = Some(result.qopn.transitions.+(tAuxiliary.id -> tAuxiliary)),
              arcs = Some(result.qopn.arcs.++(arcsToAdd))
            )
            Right(
              result.copy(
                qopn = pn,
                transitionToFinish = Some(tAuxiliary)
              )
            )
        }

      case Left(error) => Left(error)
    }
  }

  @tailrec
  private def getNextSteps(
                            config: Config, ontModel: OntModel, indvStep: Individual,
                            remainingIndvFromDivergence: Option[Individual] = None, isAfterConvergence: Boolean = false):
  Either[QOPNGenerationError, (List[Individual], Boolean)] = {
    logger.info(s"Retrieving next step from ${indvStep}.")
    // Get 'isConnectedTo' object property
    val isConnectedToProperty = ontModel.getProperty(s"${config.sfcConfig.ontoPlcConfig.ns}#isConnectedTo")
    // Retrieve the next individual that the current individual is connected to
    getIndividualsFromObjectProperty(ontModel, indvStep, isConnectedToProperty).headOption match {
      case Some(connectedToNode) =>
        val indvTConnectedTo = ontModel.getIndividual(connectedToNode.asNode().getURI)
        val sfcElChecks = for {
          trans <- isTransition(config, ontModel, indvTConnectedTo)
          simConv <- isSimultaneousConvergence(config, ontModel, indvTConnectedTo)
        } yield (trans, simConv)
        sfcElChecks match {
          case Right((isTransition, isSimultaneousConvergence)) =>

            // Check if this individual is a transition
            if (isTransition) {
              logger.info(s"Next element $indvTConnectedTo is a transition.")
              getIndividualsFromObjectProperty(ontModel, indvTConnectedTo, isConnectedToProperty).headOption match {
                case Some(step) =>
                  val nextStep = ontModel.getIndividual(step.asNode().getURI)
                  // Check if next step is simultaneous divergence
                  isSimultaneousDivergence(config, ontModel, nextStep) match {
                    case Right(b) if b =>
                      logger.info("Next element is simultaneous divergence, thus we retrieve the connected two steps and return it (we skip the simultaneous divergence node).")
                      Right((getIndividualsFromObjectProperty(ontModel, nextStep, isConnectedToProperty), false))
                    case Right(b) if !b =>
                      // No simultaneous divergence
                      // Check if terminal step
                      val hasNameDataProperty = ontModel.getDatatypeProperty(s"${config.sfcConfig.ontoPlcConfig.ns}#hasName")
                      val name = Option(nextStep.getPropertyValue(hasNameDataProperty))
                      name match {
                        case Some(n) if n.asLiteral().getString.equals("TerminalStep") => Right((List.empty), isAfterConvergence) // Is terminal step, so just return no next step
                        case _ => Right((List(nextStep), isAfterConvergence)) // No simultaneous divergence, so just return retrieve next step
                      }
                    case Left(error) => Left(error)
                  }
                case None => Right((List.empty, isAfterConvergence))
              }
            }

            // Check if this individual is a simultaneous convergence
            else if (isSimultaneousConvergence) {
              logger.info(s"Next step $indvTConnectedTo is a simultaneous convergence.")
              remainingIndvFromDivergence match {
                case Some(_) =>
                  logger.info(s"The second branch of the simultaneous divergence has not yet been processed, returning no next step.")
                  Right((List.empty, isAfterConvergence))
                case None =>
                  logger.info(s"The second branch of the simultaneous divergence has already been processed, continuing to retrieve next step...")
                  // We call this method again to retrieve the transition -> step
                  getNextSteps(config, ontModel, indvTConnectedTo, isAfterConvergence = true)
              }
            }

            else Left(QOPNGenerationError(s"Unknown SFC element (neither transition nor simultaneous convergence): $indvTConnectedTo."))

          case Left(error) => Left(error)
        }
      case None => Right((List.empty, isAfterConvergence))
    }
  }

  private def getQualityCharacteristicsFromProcessStep(config: Config, ontModel: OntModel, individual: Individual): Either[QOPNGenerationError, List[Individual]] = {
    // Get 'correspondsTo' object property
    val correspondsToProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#correspondsTo")
    // Get 'influences' object property
    val influencesProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/influences")

    // Retrieve process step
    val processStepOpt = getIndividualsFromObjectProperty(ontModel, individual, correspondsToProperty).headOption
    processStepOpt match {
      case Some(ps) =>
        // Retrieve all the quality characteristics influenced by this process step
        val qualityCharacteristicIndividuals = ps.listProperties(influencesProperty).asScala.toList
          .map(_.getObject).map(n => ontModel.getIndividual(n.asNode().getURI))
        Right(qualityCharacteristicIndividuals)
      case None => Left(QOPNGenerationError(s"Could not find corresponding process operation for step ${individual}."))
    }
  }

  private def getResourceFromProcessStep(config: Config, ontModel: OntModel, individual: Individual): Either[QOPNGenerationError, Individual] = {
    // Get 'executedBy' object property
    val executedByProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#executedBy")
    // Retrieve resource (asset)
    val resourceIndividual = individual.listProperties(executedByProperty).asScala.toList
      .map(_.getObject).headOption.map { pNode =>
      ontModel.getIndividual(pNode.asNode().getURI)
    }
    resourceIndividual
      .map(Right(_))
      .getOrElse(Left(QOPNGenerationError(s"Could not find resource that executes process step ${individual}.")))
  }


  private def getQualityConditions(config: Config, ontModel: OntModel, infModel: InfModel, individual: Individual): Either[QOPNGenerationError, List[QualityCondition]] = {
    // Get 'correspondsTo' object property
    val correspondsToProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#correspondsTo")
    val processStepOpt = getIndividualsFromObjectProperty(ontModel, individual, correspondsToProperty).headOption

    processStepOpt match {
      case Some(ps) =>
        // Get 'hasQualityCondition' object property
        val hasQualityConditionProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasQualityCondition")
        // Retrieve quality condition individuals
        val qualityConditionIndividualList = getIndividualsFromObjectProperty(ontModel, individual, hasQualityConditionProperty)

        // Get 'preConditionQualityCharacteristic' object property
        val preConditionQualityCharacteristicProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/preConditionQualityCharacteristic")
        // Get 'postConditionQualityCharacteristic' object property
        val postConditionQualityCharacteristicProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/postConditionQualityCharacteristic")
        // Get 'preConditionProcess' object property
        val preConditionProcessProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/preConditionProcess")
        // Get 'postConditionQualityCharacteristic' object property
        val postConditionProcessProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/postConditionProcess")
        // Get 'qualityConditionAppliesTo' object property
        val qualityConditionAppliesToProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualityConditionAppliesTo")


        val preAndPostConditionQcs: List[Either[QOPNGenerationError, Option[QualityCondition]]] =
          qualityConditionIndividualList.map { qcIndv =>
            // Get the referenced (pre- and post-) process steps
            val processConditionOpt = for {
              preConditionProcess <- getIndividualsFromObjectProperty(ontModel, qcIndv, preConditionProcessProperty).headOption
              postConditionProcess <- getIndividualsFromObjectProperty(ontModel, qcIndv, postConditionProcessProperty).headOption
            } yield (preConditionProcess, postConditionProcess)
            processConditionOpt match {
              // Case where current step is post process step (which were are looking for)
              case Some(processCondition) if processCondition._2.getURI.equals(ps.getURI) =>
                // Obtain quality condition individual from inference model
                val qualityConditionIndvInf = infModel.getResource(qcIndv.getURI)
                // Obtain inferred SFC step (referenced with 'qualityConditionAppliesTo')
                val preSfcStepOpt = infModel.listStatements(qualityConditionIndvInf, qualityConditionAppliesToProperty, null)
                  .asScala.map(_.getObject)
                  .filterNot(n => n.asResource().getURI.equals(individual.getURI))
                  .toList.headOption.map(n => ontModel.getIndividual(n.asResource().getURI))

                preSfcStepOpt match {
                  case Some(preSfcStepIndividual) =>

                    val preConditionQualityCharacteristics =
                      getIndividualsFromObjectProperty(ontModel, qcIndv, preConditionQualityCharacteristicProperty)
                    val postConditionQualityCharacteristics =
                      getIndividualsFromObjectProperty(ontModel, qcIndv, postConditionQualityCharacteristicProperty)

                    Right(
                      Some(
                        QualityCondition(
                          preProcessStep = processCondition._1,
                          postProcessStep = processCondition._2,
                          preQualityCharacteristics = preConditionQualityCharacteristics,
                          postQualityCharacteristics = postConditionQualityCharacteristics,
                          preSfcStep = preSfcStepIndividual
                        )
                      )
                    )

                  case None => Left(QOPNGenerationError(s"Could not retrieve individual via object property 'qualityConditionAppliesTo' from individual ${qcIndv}."))
                }

              // Case where current step is pre process step (return None, as we only create arcs from a post process step)
              case Some(processCondition) if processCondition._1.getURI.equals(ps.getURI) =>
                Right(None)
              case None => Left(QOPNGenerationError(s"Could not retrieve individuals via object properties 'preConditionProcess' and  'postConditionQualityCharacteristic' from individual ${qcIndv}."))
            }
          }

        // Check if we can collect one Left
        preAndPostConditionQcs.collectFirst { case x@Left(_) => x } match {
          case Some(Left(l)) => Left(l) // Return first error
          case None => // No error occurred, flatten list and return result
            Right(preAndPostConditionQcs.collect { case Right(Some(x)) => x })
        }
      case None => Left(QOPNGenerationError(s"Could not find corresponding process operation for step ${individual}."))
    }
  }

  private def getCorrespondingOperation(config: Config, ontModel: OntModel, individual: Individual): Either[QOPNGenerationError, Individual] = {
    // Get 'correspondsTo' object property
    val correspondsToProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#correspondsTo")
    getIndividualsFromObjectProperty(ontModel, individual, correspondsToProperty)
      .headOption
      .map(Right(_))
      .getOrElse(Left(QOPNGenerationError(s"Could not retrieve corresponding operation (manufacturing or quality control step), affects ${individual}.")))
  }


  private def processManufacturingOperation(
                                             config: Config, ontModel: OntModel, infModel: InfModel,
                                             nextIndividual: Individual, result: ProcessFlowResult,
                                             petriNet: PetriNet, stepId: String, vulnerableAssetsMap: Map[String, Boolean],
                                             processSecondBranchOfDivergence: Boolean = false,
                                             isAfterConvergence: Boolean = false
                                           ): Either[QOPNGenerationError, ProcessFlowResult] = {

    logger.info(s"Processing manufacturing operation for individual ${nextIndividual}.")

    // ========= Places =========
    val pResourceStopped = Place(id = s"p_resource_stopped_$stepId", name = s"resource_stopped_$stepId")
    val pOperation = Place(id = s"p_operation_$stepId", name = s"operation_$stepId")
    val pTransitionToNextStep = Place(id = s"p_transition_$stepId", name = s"transition_$stepId")
    val pAttackComplete = Place(id = s"p_attack_complete_$stepId", name = s"attack_complete_$stepId")
    val pOrJoin1 = Place(id = s"p_or_join_1_$stepId", name = s"or_join_1_$stepId")

    // Create places for all quality characteristics
    val (pQualityCharacteristicsOk, pQualityCharacteristicsCompromised) = getQualityCharacteristicsFromProcessStep(config, ontModel, nextIndividual) match {
      case Right(qcIndvList) =>
        qcIndvList.map { qcIndv =>
          val qcName = OntModelUtils.removeNamespace(qcIndv.getURI)
          val pQualityCharacteristicOk = Place(id = s"p_${qcName}_$stepId", name = s"${qcName}_$stepId")
          val pQualityCharacteristicCompromised = Place(id = s"p_${qcName}_compromised_$stepId", name = s"${qcName}_compromised_$stepId")
          (pQualityCharacteristicOk, pQualityCharacteristicCompromised)
        }.unzip
      case Left(error) => return Left(error)
    }

    // Create resource places
    val (pResourceReady, pResourceVulnerable, pResourceVulnerableComplement, resourceVulnerable) =
      createResourcePlaces(config, ontModel, nextIndividual, petriNet, vulnerableAssetsMap) match {
        case Right((w, x, y, z)) => (w, x, y, z)
        case Left(error) => return Left(error)
      }

    // ========= Transitions =========
    val tResourceStarts = Transition(id = s"t_resource_starts_$stepId", name = s"resource_starts_$stepId")
    val tResourceStops = Transition(id = s"t_resource_stops_$stepId", name = s"resource_stops_$stepId")
    val tAttackOnOperation = Transition(id = s"t_attack_on_operation_$stepId", name = s"attack_on_operation_$stepId")
    val tOrJoinAttackResourceStops = Transition(id = s"t_or_join_attack_resource_stops_$stepId", name = s"or_join_attack_resource_stops_$stepId")
    val tOrJoin2 = Transition(id = s"t_or_join_2_$stepId", name = s"or_join_2_$stepId")

    // Prepare quality characteristics lists to append them to the Petri net
    val qualityCharacteristicsPlaces =
      pQualityCharacteristicsOk.map(p => p.id -> p) :::
        pQualityCharacteristicsCompromised.map(p => p.id -> p)

    // Create arcs for quality characteristics
    val arcsForQualityCharacteristicsOk = pQualityCharacteristicsOk.flatMap { q =>
      SortedMap(
        (tResourceStops.id, q.id) -> Arc(s"a_resource_stops_to_${q.id}", tResourceStops, q, 1),
        (q.id, tOrJoin2.id) -> Arc(s"a_${q.id}_to_or_join_2_$stepId", q, tOrJoin2, 1),
        (tOrJoin2.id, q.id) -> Arc(s"a_or_join_2_to_${q.id}", tOrJoin2, q, 1),
      )
    }.toMap

    val arcsForQualityCharacteristicsCompromised = pQualityCharacteristicsCompromised.flatMap { q =>
      SortedMap(
        (tAttackOnOperation.id, q.id) -> Arc(s"a_attack_on_operation_to_${q.id}", tAttackOnOperation, q, 1),
        (q.id, tOrJoinAttackResourceStops.id) -> Arc(s"a_${q.id}_to_or_join_attack_resource_stops_$stepId", q, tOrJoinAttackResourceStops, 1),
        (tOrJoinAttackResourceStops.id, q.id) -> Arc(s"a_or_join_attack_resource_stops_to_${q.id}", tOrJoinAttackResourceStops, q, 1),
      )
    }.toMap

    // Create arcs to connect template of last step with current template
    val arcFromPlaceOrJoinToResourceStartsTransition =
      if (!processSecondBranchOfDivergence)
        result.placeOrJoinBeforeAuxTransition.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_or_join_1_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcFromPlaceOrJoin2ToResourceStartsTransition =
      if (processSecondBranchOfDivergence)
        result.placeOrJoinBeforeAuxTransition2.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_or_join_2_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcFromTransitionToNextStepPlaceResourceStartsTransition =
      if (!processSecondBranchOfDivergence)
        result.lastTransitionToNextStepPlace.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_transition_to_next_step_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcFromTransitionToNextStepPlace2ResourceStartsTransition =
      if (processSecondBranchOfDivergence)
        result.lastTransitionToNextStepPlace2.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_transition_to_next_step_2_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcsToAddToConnectTemplates = List(
      arcFromPlaceOrJoinToResourceStartsTransition,
      arcFromPlaceOrJoin2ToResourceStartsTransition,
      arcFromTransitionToNextStepPlaceResourceStartsTransition,
      arcFromTransitionToNextStepPlace2ResourceStartsTransition
    ).flatten

    // Create arcs to replicate a convergence element
    val arcConvergenceFromPlaceOrJoinToResourceStartsTransition =
      if (isAfterConvergence)
        result.copyOfPlaceOrJoinBeforeAuxTransition.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_or_join_3_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcConvergenceFromTransitionToNextStepPlaceResourceStartsTransition = {
      if (isAfterConvergence)
        result.copyOfLastTransitionToNextStepPlaceForFirstBranch.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_transition_to_next_step_3_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None
    }

    val arcsToAddConvergence = List(
      arcConvergenceFromPlaceOrJoinToResourceStartsTransition,
      arcConvergenceFromTransitionToNextStepPlaceResourceStartsTransition
    ).flatten

    // ========= Create elements for modeling cascading effects =========
    val cascEffects = getQualityConditions(config, ontModel, infModel, nextIndividual) match {
      case Right(qcList) =>
        qcList.flatMap { qualityCondition =>
          // Create a new transition for modeling the cascading effect
          val resStopsId = java.util.UUID.randomUUID.toString.replace("-", "_")
          val tResourceStops2 = Transition(id = s"t_resource_stops_$resStopsId", name = s"resource_stops_$resStopsId")
          // Per linked individual, retrieve place for compromised quality characteristic of previous step
          val preCompromisedQualityCharacteristicPlaces = qualityCondition.preQualityCharacteristics.flatMap { preQualIndv =>
            val qcName = OntModelUtils.removeNamespace(preQualIndv.getURI)
            val prevStepId = OntModelUtils.removeNamespace(qualityCondition.preSfcStep.getURI)
            petriNet.places.get(s"p_${qcName}_compromised_$prevStepId")
          }
          preCompromisedQualityCharacteristicPlaces.isEmpty match {
            case true => None // If there are no compromised quality characteristics in previous places, we just skip this quality condition
            case false =>
              // Create the arcs in both directions (<->) from all compromised quality characteristics to the new transition
              val prevQcComprArcs = preCompromisedQualityCharacteristicPlaces.flatMap { prePlace =>
                List(
                  (prePlace.id, tResourceStops2.id) -> Arc(s"a_${prePlace.id}_to_${tResourceStops2.id}", prePlace, tResourceStops2, 1),
                  (tResourceStops2.id, prePlace.id) -> Arc(s"a_${tResourceStops2.id}_to_${prePlace.id}", tResourceStops2, prePlace, 1)
                )
              }

              // Create arcs from transition to compromised places, transition place, OR join place, operation place, and resource ready place
              val miscArcs = List(
                (tResourceStops2.id, pResourceReady.id) -> Arc(s"a_${tResourceStops2.id}_to_${pResourceReady.id}", tResourceStops2, pResourceReady, 1),
                (pOperation.id, tResourceStops2.id) -> Arc(s"a_operation_${stepId}_to_${tResourceStops2.id}", pOperation, tResourceStops2, 1),
                (tResourceStops2.id, pOrJoin1.id) -> Arc(s"a_${tResourceStops2.id}_to_or_join_${stepId}", tResourceStops2, pOrJoin1, 1),
                (tResourceStops2.id, pTransitionToNextStep.id) -> Arc(s"a_${tResourceStops2.id}_to_next_step_${stepId}", tResourceStops2, pTransitionToNextStep, 1),
              )

              // Create arcs to the affected quality characteristic (compromised) places
              val arcsAffectedQc = qualityCondition.postQualityCharacteristics.flatMap { qcIndv =>
                val qcName = OntModelUtils.removeNamespace(qcIndv.getURI)
                pQualityCharacteristicsCompromised.find(_.id.equals(s"p_${qcName}_compromised_$stepId")).map { pqcc =>
                  (tResourceStops2.id, pqcc.id) -> Arc(s"a_${tResourceStops2.id}_to_${pqcc.id}", tResourceStops2, pqcc, 1)
                }
              }

              // Create arcs to the not compromised quality characteristics places
              val postQcIds = qualityCondition.postQualityCharacteristics
                .map(x => OntModelUtils.removeNamespace(x.getURI))
                .map(x => s"p_${x}_$stepId")
              val arcsNotAffectedQc = pQualityCharacteristicsOk.filterNot(x => postQcIds.contains(x.id)).map { pqcok =>
                (tResourceStops2.id, pqcok.id) -> Arc(s"a_${tResourceStops2.id}_to_${pqcok.id}", tResourceStops2, pqcok, 1)
              }

              // Per linked individual, retrieve place for non-compromised quality characteristic of previous step
              val preQualityCharacteristicPlaces = qualityCondition.preQualityCharacteristics.flatMap { preQualIndv =>
                val qcName = OntModelUtils.removeNamespace(preQualIndv.getURI)
                val prevStepId = OntModelUtils.removeNamespace(qualityCondition.preSfcStep.getURI)
                petriNet.places.get(s"p_${qcName}_$prevStepId")
              }
              // Create arcs in both directions (<->) from pre-quality characteristic ok to first resource stop transition to disable it
              val prevQcArcs = preQualityCharacteristicPlaces.flatMap { prePlace =>
                List(
                  (prePlace.id, tResourceStops.id) -> Arc(s"a_${prePlace.id}_to_${tResourceStops.id}", prePlace, tResourceStops, 1),
                  (tResourceStops.id, prePlace.id) -> Arc(s"a_${tResourceStops.id}_to_${prePlace.id}", tResourceStops, prePlace, 1)
                )
              }

              Some((tResourceStops2, prevQcComprArcs ::: miscArcs ::: arcsAffectedQc ::: arcsNotAffectedQc ::: prevQcArcs))
          }
        }
      case Left(error) => return Left(error)
    }

    // ========= Create elements for modeling subsequent QC check of multiple quality characteristics =========
    case class ModelingQualityCheckOfMultipleCharacteristics(
                                                              places: List[Place],
                                                              transitions: List[Transition],
                                                              arcs: SortedMap[(String, String), Arc],
                                                            )

    val (placeOrJoinBeforeNextStepTransition, qcCheckEls) = getQualityChecks(config, ontModel, infModel, nextIndividual) match {
      case Right(qualityChecks) if qualityChecks.nonEmpty =>

        if (qualityChecks.count(_.covers.size > 1) > 0) {

          // Place denoting that joining is complete
          val pQcJoinComplete = Place(id = s"p_qc_join_complete_${stepId}", name = s"qc_join_complete_${stepId}")
          val res = qualityChecks.map { qualCheck =>

            // Place denoting that at least one of the covered quality characteristics is compromised
            val pAtLeastOneCompr = Place(id = s"p_at_least_one_qc_compromised_${stepId}", name = s"at_least_one_qc_compromised_${stepId}")

            // AND join, all covered quality characteristics are ok
            val tAndAllCoveredQualityCharacteristicsOk = Transition(id = s"t_and_join_qc_ok_${stepId}", name = s"and_join_qc_ok_${stepId}")
            val pAndAllCoveredQualityCharacteristicsOk = Place(id = s"p_and_join_qc_ok_${stepId}", name = s"p_and_join_qc_ok_${stepId}")
            val arcsAndJoin = SortedMap(
              (tAndAllCoveredQualityCharacteristicsOk.id, pQcJoinComplete.id) -> Arc(id = s"a_and_join_all_qc_ok_to_qc_join_complete_${stepId}", tAndAllCoveredQualityCharacteristicsOk, pQcJoinComplete, 1),
              (pOrJoin1.id, tAndAllCoveredQualityCharacteristicsOk.id) -> Arc(id = s"a_or_join_to_and_join_all_qc_ok_${stepId}", pOrJoin1, tAndAllCoveredQualityCharacteristicsOk, 1),
              (tAndAllCoveredQualityCharacteristicsOk.id, pAndAllCoveredQualityCharacteristicsOk.id) -> Arc(id = s"a_and_join_all_qc_ok_to_or_join_${stepId}", tAndAllCoveredQualityCharacteristicsOk, pAndAllCoveredQualityCharacteristicsOk, 1),
            )

            val qcs = qualCheck.covers.map { coveredQualIndv =>
              val qcName = OntModelUtils.removeNamespace(coveredQualIndv.getURI)
              val qualPlaces = for {
                qOk <- pQualityCharacteristicsOk.find(p => p.id.equals(s"p_${qcName}_$stepId"))
                qCompr <- pQualityCharacteristicsCompromised.find(p => p.id.equals(s"p_${qcName}_compromised_$stepId"))
              } yield (qOk, qCompr)

              qualPlaces match {
                case Some((coveredQualityOk, coveredQualityCompromised)) =>

                  // Create for each compromised quality characteristic under check a new transition
                  val tQualCompr = Transition(id = s"t_or_join_${coveredQualityCompromised.id}", name = s"or_join_${coveredQualityCompromised.id}")

                  val a = SortedMap(
                    (pOrJoin1.id, tQualCompr.id) -> Arc(id = s"a_or_join_1_to_${tQualCompr.id}", pOrJoin1, tQualCompr, 1),
                    (tQualCompr.id, pQcJoinComplete.id) -> Arc(id = s"a_${tQualCompr.id}_to_${pQcJoinComplete.id}", tQualCompr, pQcJoinComplete, 1),
                    (tQualCompr.id, pAtLeastOneCompr.id) -> Arc(id = s"a_${tQualCompr.id}_to_${pAtLeastOneCompr.id}", tQualCompr, pAtLeastOneCompr, 1),
                    (coveredQualityCompromised.id, tQualCompr.id) -> Arc(id = s"a_${coveredQualityCompromised.id}_to_${tQualCompr.id}", coveredQualityCompromised, tQualCompr, 1),
                    (tQualCompr.id, coveredQualityCompromised.id) -> Arc(id = s"a_${tQualCompr.id}_to_${coveredQualityCompromised.id}", tQualCompr, coveredQualityCompromised, 1),
                    (coveredQualityOk.id, tAndAllCoveredQualityCharacteristicsOk.id) -> Arc(id = s"a_${coveredQualityOk.id}_to_${tAndAllCoveredQualityCharacteristicsOk.id}", coveredQualityOk, tAndAllCoveredQualityCharacteristicsOk, 1),
                    (tAndAllCoveredQualityCharacteristicsOk.id, coveredQualityOk.id) -> Arc(id = s"a_${tAndAllCoveredQualityCharacteristicsOk.id}_to_${coveredQualityOk.id}", tAndAllCoveredQualityCharacteristicsOk, coveredQualityOk, 1),
                  )

                  (tQualCompr, a)

                case None => return Left(QOPNGenerationError(s"Could not find quality characteristic places (non- and compromised) for ${coveredQualIndv}."))
              }
            }.unzip

            ModelingQualityCheckOfMultipleCharacteristics(
              places = List(
                pAtLeastOneCompr,
                pAndAllCoveredQualityCharacteristicsOk
              ),
              transitions = List(
                tAndAllCoveredQualityCharacteristicsOk
              ) ::: qcs._1,
              arcs = arcsAndJoin.++(qcs._2.flatten),
            )

          }

          (Some(pQcJoinComplete), res)

        }
        // None of the quality checks processed covers more than one quality characteristic, so we don't need the additional elements
        else (None, List.empty)
      case Right(qualityChecks) if qualityChecks.isEmpty => (None, List.empty)
      case Left(error) => return Left(error)
    }

    // Setup new Petri net
    val pn = PetriNet.updateUsing(
      petriNet,
      places =
        Some(
          petriNet.places
            .+(
              pResourceReady.id -> pResourceReady,
              pOperation.id -> pOperation,
              pTransitionToNextStep.id -> pTransitionToNextStep,
              pResourceVulnerable.id -> pResourceVulnerable,
              pResourceVulnerableComplement.id -> pResourceVulnerableComplement,
              pAttackComplete.id -> pAttackComplete,
              pResourceStopped.id -> pResourceStopped,
              pOrJoin1.id -> pOrJoin1
            )
            // Append quality characteristics places
            .++(qualityCharacteristicsPlaces)
            // Append places to model quality check if more than one quality characteristic is under test
            .++(qcCheckEls.flatMap(_.places.map(p => p.id -> p)))
            .++(placeOrJoinBeforeNextStepTransition.map(p => p.id -> p))
        ),
      transitions =
        Some(
          petriNet.transitions.+(
            tResourceStarts.id -> tResourceStarts,
            tResourceStops.id -> tResourceStops,
            tAttackOnOperation.id -> tAttackOnOperation,
            tOrJoinAttackResourceStops.id -> tOrJoinAttackResourceStops,
            tOrJoin2.id -> tOrJoin2
          )
            // Append transitions for modeling cascading effects
            .++(cascEffects.map(_._1).map(t => t.id -> t))
            // Append transitions to model quality check if more than one quality characteristic is under test
            .++(qcCheckEls.flatMap(_.transitions.map(t => t.id -> t)))
        ),
      marking =
        Some(
          Marking(
            petriNet.marking.places ::: List(
              pResourceReady,
              pResourceVulnerable,
              pResourceVulnerableComplement
            ),
            petriNet.marking.tokens ::: List(
              Token(1L), // Place denoting that a resource is ready
              Token(if (resourceVulnerable) 1L else 0L), // Place denoting that a resource is vulnerable
              Token(if (!resourceVulnerable) 1L else 0L), // Place denoting that a resource is not vulnerable
            )
          )
        ),
      arcs =
        Some(
          petriNet.arcs
            .+(
              (pResourceReady.id, tResourceStarts.id) -> Arc(s"a_resource_ready_to_resource_starts_$stepId", pResourceReady, tResourceStarts, 1),
              (tResourceStarts.id, pOperation.id) -> Arc(s"a_resource_starts_to_operation_$stepId", tResourceStarts, pOperation, 1),
              (pOperation.id, tResourceStops.id) -> Arc(s"a_operation_to_resource_stops_$stepId", pOperation, tResourceStops, 1),
              (tResourceStops.id, pTransitionToNextStep.id) -> Arc(s"a_resource_stops_of_${stepId}_to_transition_to_next_step", tResourceStops, pTransitionToNextStep, 1),
              (pOperation.id, tAttackOnOperation.id) -> Arc(s"a_operation_to_attack_on_operation_$stepId", pOperation, tAttackOnOperation, 1),
              (tAttackOnOperation.id, pResourceVulnerable.id) -> Arc(s"a_attack_on_operation_to_resource_vulnerable_$stepId", tAttackOnOperation, pResourceVulnerable, 1),
              (pResourceVulnerable.id, tAttackOnOperation.id) -> Arc(s"a_resource_vulnerable_to_attack_on_operation_$stepId", pResourceVulnerable, tAttackOnOperation, 1),
              (tAttackOnOperation.id, pAttackComplete.id) -> Arc(s"a_attack_on_operation_to_attack_complete_$stepId", tAttackOnOperation, pAttackComplete, 1),
              (tResourceStops.id, pResourceVulnerableComplement.id) -> Arc(s"a_resource_stops_to_resource_vulnerable_complement_$stepId", tResourceStops, pResourceVulnerableComplement, 1),
              (pResourceVulnerableComplement.id, tResourceStops.id) -> Arc(s"a_resource_vulnerable_complement_to_resource_stops_$stepId", pResourceVulnerableComplement, tResourceStops, 1),
              (tResourceStops.id, pResourceStopped.id) -> Arc(s"a_resource_stops_to_resource_stopped_$stepId", tResourceStops, pResourceStopped, 1),
              (tResourceStops.id, pResourceReady.id) -> Arc(s"a_resource_stops_to_resource_ready_$stepId", tResourceStops, pResourceReady, 1),
              (pAttackComplete.id, tOrJoinAttackResourceStops.id) -> Arc(s"a_attack_complete_to_or_join_attack_resource_stops_$stepId", pAttackComplete, tOrJoinAttackResourceStops, 1),
              (tOrJoinAttackResourceStops.id, pOrJoin1.id) -> Arc(s"a_or_join_attack_resource_stops_to_or_join_1_$stepId", tOrJoinAttackResourceStops, pOrJoin1, 1),
              (pResourceStopped.id, tOrJoin2.id) -> Arc(s"a_resource_stopped_to_or_join_2_$stepId", pResourceStopped, tOrJoin2, 1),
              (tOrJoin2.id, pOrJoin1.id) -> Arc(s"a_or_join_2_to_or_join_1_$stepId", tOrJoin2, pOrJoin1, 1),
              (tOrJoinAttackResourceStops.id, pResourceReady.id) -> Arc(s"a_or_join_attack_resource_stops_to_resource_ready_$stepId", tOrJoinAttackResourceStops, pResourceReady, 1),
              (tAttackOnOperation.id, pTransitionToNextStep.id) -> Arc(s"a_attack_on_operation_to_transition_to_next_step_$stepId", tAttackOnOperation, pTransitionToNextStep, 1),
            )
            .++(arcsForQualityCharacteristicsOk).++(arcsForQualityCharacteristicsCompromised)
            .++(arcsToAddToConnectTemplates)
            .++(cascEffects.flatMap(_._2))
            .++(qcCheckEls.flatMap(_.arcs))
            .++(arcsToAddConvergence)
        )
    )
    Right(
      ProcessFlowResult(
        qopn = pn,
        firstResourceStartsTransition = Some(result.firstResourceStartsTransition.getOrElse(tResourceStarts)),
        lastTransitionToNextStepPlace = Some(pTransitionToNextStep),
        lastTransitionToNextStepPlace2 = if (processSecondBranchOfDivergence) None else result.lastTransitionToNextStepPlace2,
        placeOrJoinBeforeAuxTransition = Some(placeOrJoinBeforeNextStepTransition.getOrElse(pOrJoin1)),
        placeOrJoinBeforeAuxTransition2 = if (processSecondBranchOfDivergence) None else result.placeOrJoinBeforeAuxTransition2,
        copyOfLastTransitionToNextStepPlaceForFirstBranch = if (isAfterConvergence) None else result.copyOfLastTransitionToNextStepPlaceForFirstBranch,
        copyOfPlaceOrJoinBeforeAuxTransition = if (isAfterConvergence) None else result.copyOfPlaceOrJoinBeforeAuxTransition
      )
    )
  }

  private def processQualityControlOperation(
                                              config: Config, ontModel: OntModel, infModel: InfModel,
                                              nextIndividual: Individual, result: ProcessFlowResult,
                                              petriNet: PetriNet, stepId: String, vulnerableAssetsMap: Map[String, Boolean],
                                              notVulnerableResourceDetectsDefectIfQualityCharacteristicIsCompromised: Boolean,
                                              processSecondBranchOfDivergence: Boolean = false,
                                              isAfterConvergence: Boolean = false
                                            ): Either[QOPNGenerationError, ProcessFlowResult] = {

    logger.info(s"Processing quality control operation for individual ${nextIndividual}.")

    // ========= Places =========
    val pOperation = Place(id = s"p_operation_$stepId", name = s"operation_$stepId")
    val pTransitionToNextStep = Place(id = s"p_transition_$stepId", name = s"transition_$stepId")
    val pDefects = petriNet.places.getOrElse(s"p_defects", Place(id = s"p_defects", name = "defects"))
    val pDefectsToAdd = if (petriNet.places.contains(pDefects.id)) List.empty else List(pDefects.id -> pDefects)
    val pDefect = Place(id = s"p_defect_$stepId", name = s"defect_$stepId")
    val pNoDefect = Place(id = s"p_no_defect_$stepId", name = s"no_defect_$stepId")
    val pUserDefinedResourceOkCanDetectDefect = Place(id = s"p_user_defined_qc_ok_can_detect_defect_$stepId", name = s"user_defined_qc_ok_can_detect_defect_$stepId")
    val pUserDefinedResourceOkCannotDetectDefect = Place(id = s"p_user_defined_qc_ok_cannot_detect_defect_$stepId", name = s"p_user_defined_qc_ok_cannot_detect_defect_$stepId")

    // Create resource places
    val (pResourceReady, pResourceVulnerable, pResourceVulnerableComplement, resourceVulnerable) =
      createResourcePlaces(config, ontModel, nextIndividual, petriNet, vulnerableAssetsMap) match {
        case Right((w, x, y, z)) => (w, x, y, z)
        case Left(error) => return Left(error)
      }

    // ========= Transitions =========
    val tResourceStarts = Transition(id = s"t_resource_starts_$stepId", name = s"resource_starts_$stepId")
    val tResourceStopsDefect = Transition(id = s"t_resource_stops_defect_$stepId", name = s"resource_stops_defect_$stepId")
    val tResourceStopsNoDefect = Transition(id = s"t_resource_stops_no_defect_$stepId", name = s"resource_stops_no_defect_$stepId")
    val tDefectDetectedResourceOkQualityCharacteristicOk = Transition(id = s"t_defect_detected_resource_ok_quality_characteristic_ok_$stepId", name = s"defect_detected_resource_ok_quality_characteristic_ok_$stepId")
    val tNoDefectDetectedResourceOkQualityCharacteristicOk = Transition(id = s"t_no_defect_detected_resource_ok_quality_characteristic_ok_$stepId", name = s"no_defect_detected_resource_ok_quality_characteristic_ok_$stepId")
    val tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised = Transition(id = s"t_no_defect_detected_resource_vulnerable_quality_characteristic_compromised_$stepId", name = s"no_defect_detected_resource_vulnerable_quality_characteristic_compromised_$stepId")
    val tDefectDetectedResourceVulnerableQualityCharacteristicOk = Transition(id = s"t_defect_detected_resource_vulnerable_quality_characteristic_ok_$stepId", name = s"defect_detected_resource_vulnerable_quality_characteristic_ok_$stepId")
    val tDefectDetectedResourceOkQualityCharacteristicCompromised = Transition(id = s"t_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", name = s"defect_detected_resource_ok_quality_characteristic_compromised_$stepId")
    val tNoDefectDetectedResourceOkQualityCharacteristicCompromised = Transition(id = s"t_no_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", name = s"no_defect_detected_resource_ok_quality_characteristic_compromised_$stepId")

    // ========= Arcs =========

    // Create arcs to connect template of last step with current template
    val arcFromPlaceOrJoinToResourceStartsTransition =
      if (!processSecondBranchOfDivergence)
        result.placeOrJoinBeforeAuxTransition.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_or_join_1_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcFromPlaceOrJoin2ToResourceStartsTransition =
      if (processSecondBranchOfDivergence)
        result.placeOrJoinBeforeAuxTransition2.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_or_join_2_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None


    val arcFromTransitionToNextStepPlaceResourceStartsTransition =
      if (!processSecondBranchOfDivergence)
        result.lastTransitionToNextStepPlace.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_transition_to_next_step_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcFromTransitionToNextStepPlace2ResourceStartsTransition =
      if (processSecondBranchOfDivergence)
        result.lastTransitionToNextStepPlace2.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_transition_to_next_step_2_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcsToAddToConnectTemplates = List(
      arcFromPlaceOrJoinToResourceStartsTransition,
      arcFromPlaceOrJoin2ToResourceStartsTransition,
      arcFromTransitionToNextStepPlaceResourceStartsTransition,
      arcFromTransitionToNextStepPlace2ResourceStartsTransition
    ).flatten

    // Create arcs to replicate a convergence element
    val arcConvergenceFromPlaceOrJoinToResourceStartsTransition =
      if (isAfterConvergence)
        result.copyOfPlaceOrJoinBeforeAuxTransition.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_or_join_3_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None

    val arcConvergenceFromTransitionToNextStepPlaceResourceStartsTransition = {
      if (isAfterConvergence)
        result.copyOfLastTransitionToNextStepPlaceForFirstBranch.map { p =>
          (p.id, tResourceStarts.id) -> Arc(s"a_transition_to_next_step_3_to_resource_starts_$stepId", p, tResourceStarts, 1)
        } else None
    }

    val arcsToAddConvergence = List(
      arcConvergenceFromPlaceOrJoinToResourceStartsTransition,
      arcConvergenceFromTransitionToNextStepPlaceResourceStartsTransition
    ).flatten

    // ========= Create arcs for modeling quality checks =========
    val arcsForQcCheck = getQualityChecks(config, ontModel, infModel, nextIndividual) match {
      case Right(qualityChecks) =>

        qualityChecks.flatMap { qualityCheck =>

          // Per linked individual, retrieve place for non- and compromised quality characteristic of previous step
          val preQualityCharacteristicPlaces =
            for {
              prevStep <- qualityCheck.preSfcStep
            } yield {
              val prevStepId = OntModelUtils.removeNamespace(prevStep.getURI)
              qualityCheck.covers.map { preQualIndv =>
                val qcName = OntModelUtils.removeNamespace(preQualIndv.getURI)
                (
                  preQualIndv,
                  petriNet.places.get(s"p_${qcName}_$prevStepId"),
                  petriNet.places.get(s"p_${qcName}_compromised_$prevStepId"),
                )
              }
            }

          // Check that we retrieved the prior manufacturing operation
          if (preQualityCharacteristicPlaces.isEmpty) return Left(QOPNGenerationError(s"Could not retrieve previous SFC step when processing ${nextIndividual}."))

          val arcs = preQualityCharacteristicPlaces match {
            case Some(x :: Nil) =>
              x match {
                case (_, Some(pQc), Some(pQcCompr)) =>
                  Some(
                    List(
                      (pQc.id, tDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_${pQc.id}_to_${tDefectDetectedResourceOkQualityCharacteristicOk.id}", pQc, tDefectDetectedResourceOkQualityCharacteristicOk, 1),
                      (tDefectDetectedResourceOkQualityCharacteristicOk.id, pQc.id) -> Arc(s"a_${tDefectDetectedResourceOkQualityCharacteristicOk.id}_to_${pQc.id}", tDefectDetectedResourceOkQualityCharacteristicOk, pQc, 1),
                      (pQc.id, tNoDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_${pQc.id}_to_${tNoDefectDetectedResourceOkQualityCharacteristicOk.id}", pQc, tNoDefectDetectedResourceOkQualityCharacteristicOk, 1),
                      (tNoDefectDetectedResourceOkQualityCharacteristicOk.id, pQc.id) -> Arc(s"a_${tNoDefectDetectedResourceOkQualityCharacteristicOk.id}_to_${pQc.id}", tNoDefectDetectedResourceOkQualityCharacteristicOk, pQc, 1),
                      (pQcCompr.id, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id) -> Arc(s"a_${pQcCompr.id}_${tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id}", pQcCompr, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, 1),
                      (tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id, pQcCompr.id) -> Arc(s"a_${tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id}_${pQcCompr.id}", tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, pQcCompr, 1),
                      (pQc.id, tDefectDetectedResourceVulnerableQualityCharacteristicOk.id) -> Arc(s"a_${pQc.id}_to_${tDefectDetectedResourceVulnerableQualityCharacteristicOk.id}", pQc, tDefectDetectedResourceVulnerableQualityCharacteristicOk, 1),
                      (tDefectDetectedResourceVulnerableQualityCharacteristicOk.id, pQc.id) -> Arc(s"a_${tDefectDetectedResourceVulnerableQualityCharacteristicOk.id}_to_${pQc.id}", tDefectDetectedResourceVulnerableQualityCharacteristicOk, pQc, 1),
                      (pQcCompr.id, tDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_${pQcCompr.id}_to_${tDefectDetectedResourceOkQualityCharacteristicCompromised.id}", pQcCompr, tDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
                      (tDefectDetectedResourceOkQualityCharacteristicCompromised.id, pQcCompr.id) -> Arc(s"a_${tDefectDetectedResourceOkQualityCharacteristicCompromised.id}_to_${pQcCompr.id}", tDefectDetectedResourceOkQualityCharacteristicCompromised, pQcCompr, 1),
                      (pQcCompr.id, tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_${pQcCompr.id}_to_${tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id}", pQcCompr, tNoDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
                      (tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id, pQcCompr.id) -> Arc(s"a_${tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id}_to_${pQcCompr.id}", tNoDefectDetectedResourceOkQualityCharacteristicCompromised, pQcCompr, 1),
                    )
                  )
                case (qcIndv, _, _) =>
                  return Left(QOPNGenerationError(s"Could not unwrap places (compromised, non-compromised) for quality characteristic ${qcIndv}. One or both of those places do not exist."))
                case _ => return Left(QOPNGenerationError(s"Error occurred when adding arcs for modeling quality checks."))
              }
            case Some(_) =>
              // Here we return None, if we cannot retrieve one of the following elements (will yield an error)
              for {
                preStep <- qualityCheck.preSfcStep
                prevStepId <- Some(OntModelUtils.removeNamespace(preStep.getURI))
                pAtLeastOneCompr <- petriNet.places.get(s"p_at_least_one_qc_compromised_${prevStepId}")
                pAndAllCoveredQualityCharacteristicsOk <- petriNet.places.get(s"p_and_join_qc_ok_${prevStepId}")
              } yield {
                List(
                  (pAndAllCoveredQualityCharacteristicsOk.id, tDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_${pAndAllCoveredQualityCharacteristicsOk.id}_to_${tDefectDetectedResourceOkQualityCharacteristicOk.id}", pAndAllCoveredQualityCharacteristicsOk, tDefectDetectedResourceOkQualityCharacteristicOk, 1),
                  (tDefectDetectedResourceOkQualityCharacteristicOk.id, pAndAllCoveredQualityCharacteristicsOk.id) -> Arc(s"a_${tDefectDetectedResourceOkQualityCharacteristicOk.id}_to_${pAndAllCoveredQualityCharacteristicsOk.id}", tDefectDetectedResourceOkQualityCharacteristicOk, pAndAllCoveredQualityCharacteristicsOk, 1),
                  (pAndAllCoveredQualityCharacteristicsOk.id, tNoDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_${pAndAllCoveredQualityCharacteristicsOk.id}_to_${tNoDefectDetectedResourceOkQualityCharacteristicOk.id}", pAndAllCoveredQualityCharacteristicsOk, tNoDefectDetectedResourceOkQualityCharacteristicOk, 1),
                  (tNoDefectDetectedResourceOkQualityCharacteristicOk.id, pAndAllCoveredQualityCharacteristicsOk.id) -> Arc(s"a_${tNoDefectDetectedResourceOkQualityCharacteristicOk.id}_to_${pAndAllCoveredQualityCharacteristicsOk.id}", tNoDefectDetectedResourceOkQualityCharacteristicOk, pAndAllCoveredQualityCharacteristicsOk, 1),
                  (pAtLeastOneCompr.id, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id) -> Arc(s"a_${pAtLeastOneCompr.id}_${tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id}", pAtLeastOneCompr, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, 1),
                  (tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id, pAtLeastOneCompr.id) -> Arc(s"a_${tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id}_${pAtLeastOneCompr.id}", tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, pAtLeastOneCompr, 1),
                  (pAndAllCoveredQualityCharacteristicsOk.id, tDefectDetectedResourceVulnerableQualityCharacteristicOk.id) -> Arc(s"a_${pAndAllCoveredQualityCharacteristicsOk.id}_to_${tDefectDetectedResourceVulnerableQualityCharacteristicOk.id}", pAndAllCoveredQualityCharacteristicsOk, tDefectDetectedResourceVulnerableQualityCharacteristicOk, 1),
                  (tDefectDetectedResourceVulnerableQualityCharacteristicOk.id, pAndAllCoveredQualityCharacteristicsOk.id) -> Arc(s"a_${tDefectDetectedResourceVulnerableQualityCharacteristicOk.id}_to_${pAndAllCoveredQualityCharacteristicsOk.id}", tDefectDetectedResourceVulnerableQualityCharacteristicOk, pAndAllCoveredQualityCharacteristicsOk, 1),
                  (pAtLeastOneCompr.id, tDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_${pAtLeastOneCompr.id}_to_${tDefectDetectedResourceOkQualityCharacteristicCompromised.id}", pAtLeastOneCompr, tDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
                  (tDefectDetectedResourceOkQualityCharacteristicCompromised.id, pAtLeastOneCompr.id) -> Arc(s"a_${tDefectDetectedResourceOkQualityCharacteristicCompromised.id}_to_${pAtLeastOneCompr.id}", tDefectDetectedResourceOkQualityCharacteristicCompromised, pAtLeastOneCompr, 1),
                  (pAtLeastOneCompr.id, tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_${pAtLeastOneCompr.id}_to_${tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id}", pAtLeastOneCompr, tNoDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
                  (tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id, pAtLeastOneCompr.id) -> Arc(s"a_${tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id}_to_${pAtLeastOneCompr.id}", tNoDefectDetectedResourceOkQualityCharacteristicCompromised, pAtLeastOneCompr, 1),
                )
              }
            case _ => Some(Nil)
          }
          arcs match {
            case Some(l) => l
            case None => return Left(QOPNGenerationError(s"Could not assemble arcs for ${qualityCheck} when multiple quality characteristics are under test."))
          }
        }

      case Left(error) => return Left(error)
    }

    // Setup new Petri net
    val pn = PetriNet.updateUsing(
      petriNet = petriNet,
      places =
        Some(
          petriNet.places
            .+(
              pOperation.id -> pOperation,
              pTransitionToNextStep.id -> pTransitionToNextStep,
              pResourceReady.id -> pResourceReady,
              pResourceVulnerable.id -> pResourceVulnerable,
              pResourceVulnerableComplement.id -> pResourceVulnerableComplement,
              pDefect.id -> pDefect,
              pNoDefect.id -> pNoDefect,
              pUserDefinedResourceOkCanDetectDefect.id -> pUserDefinedResourceOkCanDetectDefect,
              pUserDefinedResourceOkCannotDetectDefect.id -> pUserDefinedResourceOkCannotDetectDefect,
            )
            .++(pDefectsToAdd)
        ),
      transitions =
        Some(
          petriNet.transitions.+(
            tResourceStarts.id -> tResourceStarts,
            tResourceStopsDefect.id -> tResourceStopsDefect,
            tResourceStopsNoDefect.id -> tResourceStopsNoDefect,
            tDefectDetectedResourceOkQualityCharacteristicOk.id -> tDefectDetectedResourceOkQualityCharacteristicOk,
            tNoDefectDetectedResourceOkQualityCharacteristicOk.id -> tNoDefectDetectedResourceOkQualityCharacteristicOk,
            tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id -> tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised,
            tDefectDetectedResourceVulnerableQualityCharacteristicOk.id -> tDefectDetectedResourceVulnerableQualityCharacteristicOk,
            tDefectDetectedResourceOkQualityCharacteristicCompromised.id -> tDefectDetectedResourceOkQualityCharacteristicCompromised,
            tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id -> tNoDefectDetectedResourceOkQualityCharacteristicCompromised,
          )
        ),
      marking =
        Some(
          Marking(
            petriNet.marking.places ::: List(
              pResourceReady,
              pResourceVulnerable,
              pResourceVulnerableComplement,
              pUserDefinedResourceOkCanDetectDefect,
              pUserDefinedResourceOkCannotDetectDefect
            ),
            petriNet.marking.tokens ::: List(
              Token(1L),
              Token(if (resourceVulnerable) 1L else 0L), // Place denoting that a resource is vulnerable
              Token(if (!resourceVulnerable) 1L else 0L), // Place denoting that a resource is not vulnerable
              Token(if (notVulnerableResourceDetectsDefectIfQualityCharacteristicIsCompromised) 1L else 0L),
              Token(if (!notVulnerableResourceDetectsDefectIfQualityCharacteristicIsCompromised) 1L else 0L)
            )
          )
        ),
      arcs =
        Some(
          petriNet.arcs
            .+(
              (pResourceReady.id, tResourceStarts.id) -> Arc(s"a_resource_ready_to_resource_starts_$stepId", pResourceReady, tResourceStarts, 1),
              (tResourceStarts.id, pOperation.id) -> Arc(s"a_resource_starts_to_operation_$stepId", tResourceStarts, pOperation, 1),
              (tResourceStopsDefect.id, pDefects.id) -> Arc(s"a_resource_stops_no_defect_to_defects_$stepId", tResourceStopsDefect, pDefects, 1),
              (pDefect.id, tResourceStopsDefect.id) -> Arc(s"a_defect_detected_to_resource_stops_defect_detected_$stepId", pDefect, tResourceStopsDefect, 1),
              (pNoDefect.id, tResourceStopsNoDefect.id) -> Arc(s"a_no_defect_detected_to_resource_stops_no_defect_detected_$stepId", pNoDefect, tResourceStopsNoDefect, 1),
              (tResourceStopsNoDefect.id, pTransitionToNextStep.id) -> Arc(s"a_resource_stops_no_defect_to_transition_to_next_step_$stepId", tResourceStopsNoDefect, pTransitionToNextStep, 1),
              (pResourceVulnerableComplement.id, tDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_resource_vulnerable_complement_to_defect_detected_resource_ok_quality_characteristic_ok_$stepId", pResourceVulnerableComplement, tDefectDetectedResourceOkQualityCharacteristicOk, 1),
              (tDefectDetectedResourceOkQualityCharacteristicOk.id, pResourceVulnerableComplement.id) -> Arc(s"a_defect_detected_resource_ok_quality_characteristic_ok_to_resource_vulnerable_complement_$stepId", tDefectDetectedResourceOkQualityCharacteristicOk, pResourceVulnerableComplement, 1),
              (pOperation.id, tDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_operation_to_defect_detected_resource_ok_quality_characteristic_ok_${stepId}", pOperation, tDefectDetectedResourceOkQualityCharacteristicOk, 1),
              (tDefectDetectedResourceOkQualityCharacteristicOk.id, pDefect.id) -> Arc(s"a_defect_detected_resource_ok_quality_characteristic_ok_to_defect_detected_${stepId}", tDefectDetectedResourceOkQualityCharacteristicOk, pDefect, 1),
              (pResourceVulnerableComplement.id, tNoDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_resource_vulnerable_complement_to_no_defect_detected_resource_ok_quality_characteristic_ok_$stepId", pResourceVulnerableComplement, tNoDefectDetectedResourceOkQualityCharacteristicOk, 1),
              (tNoDefectDetectedResourceOkQualityCharacteristicOk.id, pResourceVulnerableComplement.id) -> Arc(s"a_no_defect_detected_resource_ok_quality_characteristic_ok_to_resource_vulnerable_complement_$stepId", tNoDefectDetectedResourceOkQualityCharacteristicOk, pResourceVulnerableComplement, 1),
              (pOperation.id, tNoDefectDetectedResourceOkQualityCharacteristicOk.id) -> Arc(s"a_operation_to_no_defect_detected_resource_ok_quality_characteristic_ok_${stepId}", pOperation, tNoDefectDetectedResourceOkQualityCharacteristicOk, 1),
              (tNoDefectDetectedResourceOkQualityCharacteristicOk.id, pNoDefect.id) -> Arc(s"a_no_defect_detected_resource_ok_quality_characteristic_ok_to_no_defect_detected_$stepId", tNoDefectDetectedResourceOkQualityCharacteristicOk, pNoDefect, 1),
              (tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id, pResourceVulnerable.id) -> Arc(s"a_no_defect_detected_resource_vulnerable_quality_characteristic_compromised_to_resource_vulnerable_$stepId", tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, pResourceVulnerable, 1),
              (pResourceVulnerable.id, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id) -> Arc(s"a_no_defect_detected_to_no_defect_detected_resource_vulnerable_quality_characteristic_compromised_$stepId", pResourceVulnerable, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, 1),
              (pOperation.id, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id) -> Arc(s"a_operation_to_no_defect_detected_resource_vulnerable_quality_characteristic_compromised_$stepId", pOperation, tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, 1),
              (tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised.id, pNoDefect.id) -> Arc(s"a_no_defect_detected_resource_vulnerable_quality_characteristic_compromised_to_no_defect_detected_$stepId", tNoDefectDetectedResourceVulnerableQualityCharacteristicCompromised, pNoDefect, 1),
              (tDefectDetectedResourceVulnerableQualityCharacteristicOk.id, pResourceVulnerable.id) -> Arc(s"a_defect_detected_resource_vulnerable_quality_characteristic_ok_to_resource_vulnerable_$stepId", tDefectDetectedResourceVulnerableQualityCharacteristicOk, pResourceVulnerable, 1),
              (pResourceVulnerable.id, tDefectDetectedResourceVulnerableQualityCharacteristicOk.id) -> Arc(s"a_defect_detected_to_no_defect_detected_resource_vulnerable_quality_characteristic_ok_$stepId", pResourceVulnerable, tDefectDetectedResourceVulnerableQualityCharacteristicOk, 1),
              (pOperation.id, tDefectDetectedResourceVulnerableQualityCharacteristicOk.id) -> Arc(s"a_operation_to_defect_detected_resource_vulnerable_quality_characteristic_ok_$stepId", pOperation, tDefectDetectedResourceVulnerableQualityCharacteristicOk, 1),
              (tDefectDetectedResourceVulnerableQualityCharacteristicOk.id, pDefect.id) -> Arc(s"a_defect_detected_resource_vulnerable_quality_characteristic_ok_to_defect_detected_$stepId", tDefectDetectedResourceVulnerableQualityCharacteristicOk, pDefect, 1),
              (pResourceVulnerableComplement.id, tDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_resource_ok_to_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", pResourceVulnerableComplement, tDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
              (tDefectDetectedResourceOkQualityCharacteristicCompromised.id, pResourceVulnerableComplement.id) -> Arc(s"a_defect_detected_resource_ok_quality_characteristic_compromised_to_resource_ok_$stepId", tDefectDetectedResourceOkQualityCharacteristicCompromised, pResourceVulnerableComplement, 1),
              (pOperation.id, tDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_operation_to_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", pOperation, tDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
              (tDefectDetectedResourceOkQualityCharacteristicCompromised.id, pDefect.id) -> Arc(s"a_defect_detected_resource_ok_quality_characteristic_compromised_to_defect_detected_$stepId", tDefectDetectedResourceOkQualityCharacteristicCompromised, pDefect, 1),
              (pUserDefinedResourceOkCanDetectDefect.id, tDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_user_defined_resource_ok_can_detect_defect_to_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", pUserDefinedResourceOkCanDetectDefect, tDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
              (tDefectDetectedResourceOkQualityCharacteristicCompromised.id, pUserDefinedResourceOkCanDetectDefect.id) -> Arc(s"a_defect_detected_resource_ok_quality_characteristic_compromised_to_user_defined_resource_ok_can_detect_defect_$stepId", tDefectDetectedResourceOkQualityCharacteristicCompromised, pUserDefinedResourceOkCanDetectDefect, 1),
              (pResourceVulnerableComplement.id, tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_resource_ok_to_no_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", pResourceVulnerableComplement, tNoDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
              (tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id, pResourceVulnerableComplement.id) -> Arc(s"a_no_defect_detected_resource_ok_quality_characteristic_compromised_to_resource_ok_$stepId", tNoDefectDetectedResourceOkQualityCharacteristicCompromised, pResourceVulnerableComplement, 1),
              (pOperation.id, tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_operation_to_no_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", pOperation, tNoDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
              (tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id, pNoDefect.id) -> Arc(s"a_no_defect_detected_resource_ok_quality_characteristic_compromised_to_no_defect_$stepId", tNoDefectDetectedResourceOkQualityCharacteristicCompromised, pNoDefect, 1),
              (pUserDefinedResourceOkCannotDetectDefect.id, tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id) -> Arc(s"a_user_defined_resource_ok_cannot_detect_defect_to_no_defect_detected_resource_ok_quality_characteristic_compromised_$stepId", pUserDefinedResourceOkCannotDetectDefect, tNoDefectDetectedResourceOkQualityCharacteristicCompromised, 1),
              (tNoDefectDetectedResourceOkQualityCharacteristicCompromised.id, pUserDefinedResourceOkCannotDetectDefect.id) -> Arc(s"a_no_defect_detected_resource_ok_quality_characteristic_compromised_to_user_defined_resource_ok_cannot_detect_defect_$stepId", tNoDefectDetectedResourceOkQualityCharacteristicCompromised, pUserDefinedResourceOkCannotDetectDefect, 1),
              (tResourceStopsDefect.id, pResourceReady.id) -> Arc(s"a_resource_stops_defect_to_resource_ready_$stepId", tResourceStopsDefect, pResourceReady, 1),
              (tResourceStopsNoDefect.id, pResourceReady.id) -> Arc(s"a_resource_stops_no_defect_to_resource_ready_$stepId", tResourceStopsNoDefect, pResourceReady, 1),
            )
            .++(arcsToAddToConnectTemplates)
            .++(arcsForQcCheck)
            .++(arcsToAddConvergence)
        )
    )
    Right(
      ProcessFlowResult(
        qopn = pn,
        firstResourceStartsTransition = Some(result.firstResourceStartsTransition.getOrElse(tResourceStarts)),
        lastTransitionToNextStepPlace = Some(pTransitionToNextStep),
        lastTransitionToNextStepPlace2 = if (processSecondBranchOfDivergence) None else result.lastTransitionToNextStepPlace2,
        // Since this is not a manufacturing operation, we can leave this to None
        // We need to set this only if we want to create an arc from an OR join (only applicable to man. operations) to the auxiliary transition or resource starts transition
        placeOrJoinBeforeAuxTransition = None,
        copyOfLastTransitionToNextStepPlaceForFirstBranch = if (isAfterConvergence) None else result.copyOfLastTransitionToNextStepPlaceForFirstBranch,
        copyOfPlaceOrJoinBeforeAuxTransition = if (isAfterConvergence) None else result.copyOfPlaceOrJoinBeforeAuxTransition
      )
    )

  }

  private def getQualityChecks(config: Config, ontModel: OntModel, infModel: InfModel, individual: Individual): Either[QOPNGenerationError, List[QualityCheck]] = {
    // Get 'correspondsTo' object property
    val correspondsToProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#correspondsTo")
    val processStepOpt = getIndividualsFromObjectProperty(ontModel, individual, correspondsToProperty).headOption

    processStepOpt match {
      case Some(ps) =>
        // Get 'hasQualityCheck' object property
        val hasQualityCheckProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#hasQualityCheck")
        // Retrieve quality check individuals
        val qualityCheckIndividualList = getIndividualsFromObjectProperty(ontModel, individual, hasQualityCheckProperty)

        // Get 'covers' object property
        val coversProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/covers")
        // Get 'isCoveredBy' object property
        val isCoveredByProperty = ontModel.getProperty(s"${config.qualOntConfig.ns}/isCoveredBy")
        // Get 'qualityCheckAppliesTo' object property
        val qualityCheckAppliesToProperty = ontModel.getProperty(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#qualityCheckAppliesTo")

        val qualityChecks: List[Either[QOPNGenerationError, Option[QualityCheck]]] =
          qualityCheckIndividualList.map { qcIndv =>
            // Get the referenced 'covers' and 'isCoveredBy' individuals
            val coveredOpt = for {
              covers <- Some(getIndividualsFromObjectProperty(ontModel, qcIndv, coversProperty))
              isCoveredBy <- getIndividualsFromObjectProperty(ontModel, qcIndv, isCoveredByProperty).headOption
            } yield (covers, isCoveredBy)
            coveredOpt match {
              case Some(covered) =>
                // Obtain quality check individual from inference model
                val qualityCheckIndvInf = infModel.getResource(qcIndv.getURI)

                // Obtain inferred SFC step (referenced with 'qualityCheckAppliesTo')
                val preSfcStepOpt = if (covered._2.getURI.equals(ps.getURI)) // Case where current step is quality control (which were are looking for)
                  infModel.listStatements(qualityCheckIndvInf, qualityCheckAppliesToProperty, null)
                    .asScala.map(_.getObject)
                    .filterNot(n => n.asResource().getURI.equals(individual.getURI))
                    .toList.headOption.map(n => ontModel.getIndividual(n.asResource().getURI))
                else None // Case where current step is manufacturing operation

                Right(
                  Some(
                    QualityCheck(
                      covers = covered._1,
                      isCoveredBy = covered._2,
                      preSfcStep = preSfcStepOpt
                    )
                  )
                )

              case None => Left(QOPNGenerationError(s"Could not retrieve individuals via object properties 'covers' and  'isCoveredBy' from individual ${qcIndv}."))
            }
          }

        // Check if we can collect one Left
        qualityChecks.collectFirst { case x@Left(_) => x } match {
          case Some(Left(l)) => Left(l) // Return first error
          case None => // No error occurred, flatten list and return result
            Right(qualityChecks.collect { case Right(Some(x)) => x })
        }
      case None => Left(QOPNGenerationError(s"Could not find corresponding process operation for step ${individual}."))
    }
  }

  private def createResourcePlaces(
                                    config: Config, ontModel: OntModel,
                                    nextIndividual: Individual, petriNet: PetriNet,
                                    vulnerableAssetsMap: Map[String, Boolean]
                                  ): Either[QOPNGenerationError, (Place, Place, Place, Boolean)] = {
    // Create resource places
    getResourceFromProcessStep(config, ontModel, nextIndividual) match {
      case Right(rIndv) =>
        val resourceName = OntModelUtils.removeNamespace(rIndv.getURI)
        val pResourceReadyName = s"${resourceName}_ready"
        // Note: We need to create a place for resource stopped per process step and not per resource
        // Thus, we do not create the place for resource stopped here...
        val pResourceVulnerableName = s"${resourceName}_vulnerable"
        val pResourceVulnerableComplementName = s"${resourceName}_vulnerable_complement"

        val isVulnerableOpt = vulnerableAssetsMap.get(resourceName)
        isVulnerableOpt match {
          case Some(isVulnerable) =>
            List(pResourceReadyName, pResourceVulnerableName, pResourceVulnerableComplementName)
              .map { x =>
                petriNet.places.getOrElse(s"p_$x", Place(id = s"p_$x", name = x))
              } match {
              case List(a, b, c) => Right((a, b, c, isVulnerable))
            }
          case None => Left(QOPNGenerationError(s"Could not retrieve vulnerability information from Map for asset $resourceName."))
        }
      case Left(error) => Left(error)
    }
  }

  private def getIndividualsFromObjectProperty(ontModel: OntModel, individual: Individual, property: Property): List[Individual] =
    individual.listProperties(property).asScala.toList
      .map(_.getObject).map { pNode =>
      ontModel.getIndividual(pNode.asNode().getURI)
    }

  private def isSimultaneousDivergence(config: Config, ontModel: OntModel, individual: Individual): Either[QOPNGenerationError, Boolean] =
    hasClassOf(config, ontModel, individual, s"${config.sfcConfig.ontoPlcConfig.ns}#SimultaneousDivergence")

  private def isSimultaneousConvergence(config: Config, ontModel: OntModel, individual: Individual): Either[QOPNGenerationError, Boolean] =
    hasClassOf(config, ontModel, individual, s"${config.sfcConfig.ontoPlcConfig.ns}#SimultaneousConvergence")

  private def isTransition(config: Config, ontModel: OntModel, individual: Individual): Either[QOPNGenerationError, Boolean] =
    hasClassOf(config, ontModel, individual, s"${config.sfcConfig.ontoPlcConfig.ns}#Transition")

  private def hasClassOf(config: Config, ontModel: OntModel, individual: Individual, uriCls: String): Either[QOPNGenerationError, Boolean] = {
    val classOpt = Option(ontModel.getOntClass(uriCls))
    classOpt
      .map(cls => Right(individual.hasOntClass(cls)))
      .getOrElse(Left(QOPNGenerationError(s"Could not retrieve class $uriCls.")))
  }

}
