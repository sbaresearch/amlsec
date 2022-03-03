package org.sba_research.sfc

import com.typesafe.scalalogging.Logger
import org.apache.jena.datatypes.RDFDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.ontology.{Individual, OntModel, OntModelSpec}
import org.apache.jena.rdf.model.{ModelFactory, ResourceFactory}
import org.apache.jena.sparql.core.Var
import org.apache.jena.sparql.engine.binding.Binding
import org.sba_research.Config
import org.sba_research.utils.{OntModelUtils, QueryExecutor}
import scalaxb.DataRecord
import www.plcopen.org.xml.tc60201.{Action, Action2, ActionBlock, Bodyable, BodyableOption, Connection, FormattedText, Inline, JumpStep, MacroStep, Pou, Program, Project, Reference2, SFC2, SelectionConvergence, SelectionDivergence, SimultaneousConvergence, SimultaneousDivergence, Step, Transition, Transition2}

import scala.xml.Elem

case class OntoPlcSemanticLiftingError(message: String)

case class OntoPlcSemanticParsingResult(model: OntModel, connectionReferences: List[OntoPlcConnectionReference] = List.empty)

sealed trait OntoPlcConnectionReference

trait ConnectionPointReference extends OntoPlcConnectionReference {
  val individual1LocalId: String
  val individual2Uri: String
}

case class ConnectionStepToActionBlock(individual1LocalId: String, individual2Uri: String) extends ConnectionPointReference

case class ConnectionTo(individual1LocalId: String, individual2Uri: String) extends ConnectionPointReference

case class ConnectionActionContainsAction(action1Uri: String, action2Name: String) extends OntoPlcConnectionReference

case class ConnectionTransitionContainsTransition(transition1Uri: String, transition2Name: String) extends OntoPlcConnectionReference

object OntoPlcSemanticLifting {

  val logger: Logger = Logger(getClass)

  // Ontology qualifiers
  val hasName = "hasName"
  val hasGlobalId = "hasGlobalId"
  val hasDuration = "hasDuration"
  val hasExecutionOrderId = "hasExecutionOrderId"
  val isInitialStep = "isInitialStep"
  val hasLocalId = "hasLocalId"
  val hasLanguage = "hasLanguage"
  val hasVersion = "hasVersion"
  val hasCreationDateTime = "hasCreationDateTime"
  val hasModificationDateTime = "hasModificationDateTime"
  val hasQualifier = "hasQualifier"

  private def createOntModel(config: Config): OntModel = {
    OntModelUtils.setupLocationMappings(config)
    val m = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM)
    val ont = m.createOntology(config.sfcConfig.sfcTransformationOntConfig.ns)
    ont.addImport(m.createResource(config.sfcConfig.ontoPlcConfig.ns))
    m.setDynamicImports(true) // Enable import processing
    m.loadImports()
    // cf. https://stackoverflow.com/a/17447438/5107545
    // `individual.listOntClasses(false).asScala.toList` throws
    // org.apache.jena.ontology.ConversionException: Cannot convert node http://www.w3.org/2002/07/owl#NamedIndividual to OntClass: it does not have rdf:type owl:Class or equivalent
    m.setStrictMode(false)
    m
  }

  private def createProjectMetaData(config: Config, prevParsingResult: OntoPlcSemanticParsingResult, project: Project): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
    try {
      // Create Project individual
      val projectClass = prevParsingResult.model.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Project")
      val projectIndividual = prevParsingResult.model.createIndividual(getProjectIndividualUri(config, project.contentHeader.name), projectClass)
      // Add 'hasName' data property
      addDataPropertyToIndividual(config, prevParsingResult, projectIndividual, hasName, project.contentHeader.name, XSDDatatype.XSDstring) match {
        case Right(rHasName) =>
          project.contentHeader.language.map { lang =>
            // Add 'hasLanguage' data property
            addDataPropertyToIndividual(config, rHasName, projectIndividual, hasLanguage, lang, XSDDatatype.XSDstring)
          }.getOrElse(Right(rHasName)) match {
            case Right(rHasLanguage) =>
              project.contentHeader.version.map { version =>
                // Add 'hasVersion' data property
                addDataPropertyToIndividual(config, rHasLanguage, projectIndividual, hasVersion, version, XSDDatatype.XSDstring)
              }.getOrElse(Right(rHasLanguage)) match {
                case Right(rHasVersion) =>
                  // Add 'hasCreationDateTime' data property
                  addDataPropertyToIndividual(config, rHasVersion, projectIndividual, hasCreationDateTime, project.fileHeader.creationDateTime.toXMLFormat, XSDDatatype.XSDdateTime)
                  match {
                    case Right(rHasCreationDateTime) =>
                      project.contentHeader.modificationDateTime.map { mdt =>
                        // Add 'hasModificationDateTime' data property
                        addDataPropertyToIndividual(config, rHasCreationDateTime, projectIndividual, hasModificationDateTime, mdt.toXMLFormat, XSDDatatype.XSDdateTime)
                      }.getOrElse(Right(rHasCreationDateTime))
                    case l@Left(_) => l
                  }
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case l@Left(_) => l
      }
    } catch {
      case e: Throwable =>
        val err = OntoPlcSemanticLiftingError("Could not create project metadata.")
        logger.error(err.message, e)
        Left(err)
    }

  private def createProgramOrganizationUnit(config: Config, prevParsingResult: OntoPlcSemanticParsingResult, project: Project, pou: Pou): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

    def createBodyElement(r1: OntoPlcSemanticParsingResult, pou: Pou, body: Bodyable): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
      try {
        val pouIndividual = r1.model.getIndividual(getPouIndividualUri(config, pou.pouType.toString.toLowerCase, pou.name))
        addBody(config, r1, pouIndividual, body.bodyableoption)
      } catch {
        case e: Throwable =>
          val err = OntoPlcSemanticLiftingError("Could not create body element.")
          logger.error(err.message, e)
          Left(err)
      }

    def createAction(r1: OntoPlcSemanticParsingResult, pou: Pou, action: Action): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
      try {
        // Create Action individual
        val actionClass = r1.model.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Action")
        val actionIndividual = r1.model.createIndividual(getActionIndividualUri(config, action.name), actionClass)
        for {
          // Add 'hasName' data property
          rHasName <- addDataPropertyToIndividual(config, r1, actionIndividual, "hasName", action.name, XSDDatatype.XSDstring)
          // Add action body ('hasStatement' data property)
          rHasBody <- addBody(config, rHasName, actionIndividual, action.body.bodyableoption)
        } yield rHasBody
      } catch {
        case e: Throwable =>
          val err = OntoPlcSemanticLiftingError("Could not create POU actions.")
          logger.error(err.message, e)
          Left(err)
      }

    def createTransition(r1: OntoPlcSemanticParsingResult, pou: Pou, transition: Transition): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
      try {
        // Create Transition individual
        val transitionClass = r1.model.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Transition")
        val transitionIndividual = r1.model.createIndividual(getTransitionIndividualUri(config, transition.name), transitionClass)
        for {
          // Add 'hasName' data property
          rHasName <- addDataPropertyToIndividual(config, r1, transitionIndividual, "hasName", transition.name, XSDDatatype.XSDstring)
          // Add transition body ('hasStatement' data property)
          rHasBody <- addBody(config, rHasName, transitionIndividual, transition.body.bodyableoption)
        } yield rHasBody
      } catch {
        case e: Throwable =>
          val err = OntoPlcSemanticLiftingError("Could not create POU actions.")
          logger.error(err.message, e)
          Left(err)
      }

    try {
      pou.pouType match {
        case Program =>
          // Create Program POU individual
          val programClass = prevParsingResult.model.getOntClass(s"${config.sfcConfig.ontoPlcConfig.ns}#Program")
          val programIndividual = prevParsingResult.model.createIndividual(getPouIndividualUri(config, pou.pouType.toString.toLowerCase, pou.name), programClass)
          // Add 'hasName' data property
          addDataPropertyToIndividual(config, prevParsingResult, programIndividual, hasName, pou.name, XSDDatatype.XSDstring) match {
            case Right(rHasName) =>
              // Global ID is optional
              pou.globalId.map { globalId =>
                // Add 'hasGlobalId' data property
                addDataPropertyToIndividual(config, rHasName, programIndividual, hasGlobalId, globalId, XSDDatatype.XSDstring)
              }
                .getOrElse(Right(rHasName)) match {
                case Right(rGlobalId) =>
                  // Add 'hasPOU' object property
                  val hasPouObjectProperty = rGlobalId.model.getObjectProperty(s"${config.sfcConfig.ontoPlcConfig.ns}#hasPOU")
                  val projectIndividual = rGlobalId.model.getIndividual(s"${config.sfcConfig.sfcTransformationOntConfig.ns}#project_${project.contentHeader.name}")
                  projectIndividual.addProperty(hasPouObjectProperty, programIndividual)
                  // Process POU actions
                  pou.actions.map { a =>
                    // Create actions
                    LazyList.unfold((rGlobalId, pou, a.action.toList)) { case (res, p, recs) =>
                      recs.headOption.map { rec =>
                        createAction(res, p, rec).fold(Left(_) -> (res, p, Nil), r => Right(r) -> (r, p, recs.tail))
                      }
                    }.last
                  }.getOrElse(Right(rGlobalId)) match {
                    case Right(rActions) =>
                      // Process POU transitions
                      pou.transitions.map { t =>
                        // Create transitions
                        LazyList.unfold((rActions, pou, t.transition.toList)) { case (res, p, recs) =>
                          recs.headOption.map { rec =>
                            createTransition(res, p, rec).fold(Left(_) -> (res, p, Nil), r => Right(r) -> (r, p, recs.tail))
                          }
                        }.last
                      }.getOrElse(Right(rActions)) match {
                        case Right(rTransitions) =>
                          // Create body elements
                          LazyList.unfold((rTransitions, pou, pou.body.toList)) { case (res, p, recs) =>
                            recs.headOption.map { rec =>
                              createBodyElement(res, p, rec).fold(Left(_) -> (res, p, Nil), r => Right(r) -> (r, p, recs.tail))
                            }
                          }.last
                        case l@Left(_) => l
                      }
                    case l@Left(_) => l
                  }
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case _ => throw new NotImplementedError("Currently, only program POU types can be translated.")
      }
    } catch {
      case e: Throwable =>
        val err = OntoPlcSemanticLiftingError("Could not create program organization units.")
        logger.error(err.message, e)
        Left(err)
    }
  }

  private def createProgramOrganizationUnits(config: Config, prevParsingResult: OntoPlcSemanticParsingResult, project: Project, pous: List[Pou]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
    LazyList.unfold((config, prevParsingResult, project, pous)) { case (c, res, p, recs) =>
      recs.headOption.map { rec =>
        createProgramOrganizationUnit(c, res, p, rec).fold(Left(_) -> (c, res, p, Nil), r => Right(r) -> (c, r, p, recs.tail))
      }
    }.last

  def createReferences(config: Config, prevParsingResult: OntoPlcSemanticParsingResult): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

    def createStepToActionBlockReferences(pr: OntoPlcSemanticParsingResult, refs: List[ConnectionStepToActionBlock]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

      def createHasActionBlockReference(parsRes: OntoPlcSemanticParsingResult, variables: List[Var], binding: Binding): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
        val model = parsRes.model
        val values = for {
          stepV <- variables.find(_.getVarName == "step").map(binding.get)
          actionBlockV <- variables.find(_.getVarName == "actionBlock").map(binding.get)
          stepIndividual <- Option(model.getIndividual(stepV.getURI))
          actionBlockIndividual <- Option(model.getIndividual(actionBlockV.getURI))
          hasActionBlockObjectProperty <- Option(model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#hasActionBlock"))
        } yield (stepIndividual, hasActionBlockObjectProperty, actionBlockIndividual)
        values.map {
          case (s, p, o) =>
            s.addProperty(p, o)
            logger.info(s"Created <${
              s.getURI
            }> hasActionBlock <${
              o.getURI
            }>.")
            Right(OntoPlcSemanticParsingResult(model = model))
        }.getOrElse(Left(OntoPlcSemanticLiftingError(s"Could not process query results $binding.")))
      }

      val model = pr.model
      // Create VALUES part
      val valuesString = refs.map(r => s"( ${
        r.individual1LocalId
      } <${
        r.individual2Uri
      }> )").mkString("\n")
      // This query is only executed once to retrieve all steps - action block pairs
      val queryResult = QueryExecutor.query(
        s =
          s"""
             |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             |PREFIX owl: <http://www.w3.org/2002/07/owl#>
             |PREFIX ontoPlc: <${
            config.sfcConfig.ontoPlcConfig.ns
          }#>
             |SELECT DISTINCT ?step ?actionBlock
             |   WHERE {
             |      ?step a ontoPlc:Step ;
             |            ontoPlc:hasLocalId ?stepLocalId .
             |      VALUES ( ?stepLocalId ?actionBlock ) {
             |           $valuesString
             |      }
             |   }
                """.stripMargin,
        model = model,
        resBinding = None
      )
      // Process query results
      LazyList.unfold((pr, queryResult.variables, queryResult.values)) {
        case (parsRes, vars, vals) =>
          vals.headOption.map {
            v =>
              createHasActionBlockReference(parsRes, vars, v).fold(Left(_) -> (parsRes, Nil, Nil), r => Right(r) -> (r, vars, vals.tail))
          }
      }.lastOption.getOrElse(Left(OntoPlcSemanticLiftingError("Empty query result when retrieving step - action block pairs.")))
    }

    def createActionToActionReferences(pr: OntoPlcSemanticParsingResult, refs: List[ConnectionActionContainsAction]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

      def createHasActionBlockReference(parsRes: OntoPlcSemanticParsingResult, variables: List[Var], binding: Binding): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
        val model = parsRes.model
        val values = for {
          action1V <- variables.find(_.getVarName == "action1").map(binding.get)
          action2V <- variables.find(_.getVarName == "action2").map(binding.get)
          actionIndividual <- Option(model.getIndividual(action1V.getURI))
          referencedActionIndividual <- Option(model.getIndividual(action2V.getURI))
          containsObjectProperty <- Option(model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#contains"))
        } yield (actionIndividual, containsObjectProperty, referencedActionIndividual)
        values.map {
          case (s, p, o) =>
            // Add 'contains' object property
            s.addProperty(p, o)
            logger.info(s"Created <${
              s.getURI
            }> contains <${
              o.getURI
            }>.")
            Right(OntoPlcSemanticParsingResult(model = model))
        }.getOrElse(Left(OntoPlcSemanticLiftingError(s"Could not process query results $binding.")))
      }

      // Create VALUES part
      val valuesString = refs.map(r =>
        s"""( <${
          r.action1Uri
        }> "${
          r.action2Name
        }"^^xsd:string )""").mkString("\n")
      // This query is only executed once to retrieve all action1 - action2 pairs
      val queryResult = QueryExecutor.query(
        s =
          s"""
             |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
             |PREFIX owl: <http://www.w3.org/2002/07/owl#>
             |PREFIX ontoPlc: <${
            config.sfcConfig.ontoPlcConfig.ns
          }#>
             |SELECT DISTINCT ?action1 ?action2
             |   WHERE {
             |      ?action2 a ontoPlc:Action ;
             |              ontoPlc:hasName ?action2Name .
             |      VALUES ( ?action1 ?action2Name ) {
             |            $valuesString
             |      }
             |   }
             """.stripMargin,
        model = pr.model,
        resBinding = None
      )
      // Process query results
      LazyList.unfold((pr, queryResult.variables, queryResult.values)) {
        case (parsRes, vars, vals) =>
          vals.headOption.map {
            v =>
              createHasActionBlockReference(parsRes, vars, v).fold(Left(_) -> (parsRes, Nil, Nil), r => Right(r) -> (r, vars, vals.tail))
          }
      }.lastOption.getOrElse(Left(OntoPlcSemanticLiftingError("Empty query result when retrieving action1 - action2 pairs.")))
    }

    def createTransitionToTransitionReferences(pr: OntoPlcSemanticParsingResult, refs: List[ConnectionTransitionContainsTransition]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

      def createTransitionContainsTransitionReference(parsRes: OntoPlcSemanticParsingResult, ref: ConnectionTransitionContainsTransition): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
        val triples = for {
          trans1 <- Option(parsRes.model.getIndividual(ref.transition1Uri))
          trans2 <- Option(parsRes.model.getIndividual(getTransitionIndividualUri(config, ref.transition2Name)))
          containsObjectProperty <- Option(parsRes.model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#contains"))
        } yield (trans1, containsObjectProperty, trans2)
        triples match {
          case Some((s, p, o)) =>
            // Add 'contains' object property
            s.addProperty(p, o)
            logger.info(s"Created <${
              s.getURI
            }> contains <${
              o.getURI
            }>.")
            Right(parsRes)
          case None => Left(OntoPlcSemanticLiftingError(s"Could not find referenced individuals [$ref]."))
        }
      }

      LazyList.unfold((pr, refs)) {
        case (parsRes, references) =>
          references.headOption.map {
            ref =>
              createTransitionContainsTransitionReference(parsRes, ref).fold(Left(_) -> (parsRes, Nil), r => Right(r) -> (r, references.tail))
          }
      }.last
    }

    def createConnectionToReferences(pr: OntoPlcSemanticParsingResult, refs: List[ConnectionTo]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

      def createConnectionReference(parsRes: OntoPlcSemanticParsingResult, variables: List[Var], binding: Binding): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
        val model = parsRes.model
        val triples = for {
          indv1V <- variables.find(_.getVarName == "indv1").map(binding.get)
          indv2V <- variables.find(_.getVarName == "indv2").map(binding.get)
          indv1 <- Option(model.getIndividual(indv1V.getURI))
          indv2 <- Option(model.getIndividual(indv2V.getURI))
          conn <- Option(model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#isConnectedTo"))
        } yield (indv1, conn, indv2)
        triples.map {
          case (s, p, o) =>
            s.addProperty(p, o)
            logger.info(s"Created <${
              s.getURI
            }> isConnectedTo <${
              o.getURI
            }>.")
            Right(OntoPlcSemanticParsingResult(model = model))
        }.getOrElse(Left(OntoPlcSemanticLiftingError("Could not retrieve triples from query result.")))
      }

      // Create VALUES part
      val valuesString = refs.map(r =>
        s"""( "${
          r.individual1LocalId
        }"^^xsd:integer <${
          r.individual2Uri
        }> )""").mkString("\n")
      // This query is only executed once to retrieve all indv1 - indv2 pairs
      val queryResult = QueryExecutor.query(
        s =
          s"""
             |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
             |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
             |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
             |PREFIX owl: <http://www.w3.org/2002/07/owl#>
             |PREFIX ontoPlc: <${
            config.sfcConfig.ontoPlcConfig.ns
          }#>
             |SELECT DISTINCT ?indv1 ?indv2
             |   WHERE {
             |      ?indv1 ontoPlc:hasLocalId ?indv1LocalId .
             |      VALUES ( ?indv1LocalId ?indv2 ) {
             |            $valuesString
             |      }
             |   }
             """.stripMargin,
        model = pr.model,
        resBinding = None
      )
      // Process query results
      LazyList.unfold((pr, queryResult.variables, queryResult.values)) {
        case (parsRes, vars, vals) =>
          vals.headOption.map {
            v =>
              createConnectionReference(parsRes, vars, v).fold(Left(_) -> (parsRes, Nil, Nil), r => Right(r) -> (r, vars, vals.tail))
          }
      }.lastOption.getOrElse(Left(OntoPlcSemanticLiftingError("Empty query result when retrieving indv1 - indv2 pairs (for connectedTo).")))
    }

    // First, partition list of references per type
    val emptyReferences:
      (
        List[ConnectionStepToActionBlock],
          List[ConnectionActionContainsAction],
          List[ConnectionTransitionContainsTransition],
          List[ConnectionTo]
        ) = (Nil, Nil, Nil, Nil)
    val references = prevParsingResult.connectionReferences.foldRight(emptyReferences) {
      case (refS2Ab@ConnectionStepToActionBlock(_, _), (s2ab, a2a, t2t, connTo)) =>
        (refS2Ab :: s2ab, a2a, t2t, connTo)
      case (refA2A@ConnectionActionContainsAction(_, _), (s2ab, a2a, t2t, connTo)) =>
        (s2ab, refA2A :: a2a, t2t, connTo)
      case (refT2T@ConnectionTransitionContainsTransition(_, _), (s2ab, a2a, t2t, connTo)) =>
        (s2ab, a2a, refT2T :: t2t, connTo)
      case (refConnTo@ConnectionTo(_, _), (s2ab, a2a, t2t, connTo)) =>
        (s2ab, a2a, t2t, refConnTo :: connTo)
      case (u, (s2ab, a2a, t2t, connTo)) =>
        logger.warn(s"Could not match $u to any connection type.")
        (s2ab, a2a, t2t, connTo)
    }
    for {
      r1 <- createStepToActionBlockReferences(prevParsingResult, references._1)
      r2 <- createActionToActionReferences(r1, references._2)
      r3 <- createTransitionToTransitionReferences(r2, references._3)
      r4 <- createConnectionToReferences(r3, references._4)
    } yield r4
  }

  private def addBody(
                       config: Config,
                       prevParsingResult: OntoPlcSemanticParsingResult,
                       individual: Individual,
                       bodyableOption: DataRecord[BodyableOption]
                     ): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

    def createSfcElement(r2: OntoPlcSemanticParsingResult, r: DataRecord[_]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {

      def createAction(r5: OntoPlcSemanticParsingResult, actionBlockIndividual: Individual, a: Action2): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
        // To create an action that is referenced by an action block, we take the global ID (or generate a UUID)
        val id = a.globalId.getOrElse(java.util.UUID.randomUUID().toString)
        // Create action individual
        val actionClass = r5.model.getOntClass(s"${
          config.sfcConfig.ontoPlcConfig.ns
        }#Action")
        val actionIndividual = r5.model.createIndividual(getActionIndividualUri(config, id), actionClass)
        // Add 'hasAction' object property
        val hasActionProperty = r5.model.getObjectProperty(s"${
          config.sfcConfig.ontoPlcConfig.ns
        }#hasAction")
        actionBlockIndividual.addProperty(hasActionProperty, actionIndividual)

        // Add 'hasLocalId' data property
        addDataPropertyToIndividual(config, r5, actionIndividual, hasLocalId, a.localId.toString(), XSDDatatype.XSDinteger) match {
          case Right(rHasLocalId) =>
            // Add 'hasGlobalId' data property
            a.globalId.map { globalId =>
              addDataPropertyToIndividual(config, rHasLocalId, actionIndividual, hasGlobalId, globalId, XSDDatatype.XSDstring)
            }.getOrElse(Right(rHasLocalId)) match {
              case Right(rlid) =>
                // Get 'hasQualifier' object property
                val hasQualifierObjectProperty = rlid.model.getObjectProperty(s"${
                  config.sfcConfig.ontoPlcConfig.ns
                }#hasQualifier")
                // Get qualifier individual
                Option(rlid.model.getIndividual(s"${
                  config.sfcConfig.ontoPlcConfig.ns
                }#${
                  a.qualifier.toString
                }")).map {
                  qualifierIndividual =>
                    // Add 'hasQualifier' object property
                    actionIndividual.addProperty(hasQualifierObjectProperty, qualifierIndividual)
                    Right(rlid)
                }.getOrElse(
                  Left(OntoPlcSemanticLiftingError(s"Could not retrieve qualifier [${
                    a.qualifier.toString
                  }] from ontology model."))
                ) match {
                  case Right(rQual) =>
                    // Duration is optional
                    a.duration.map {
                      duration =>
                        // Add 'hasDuration' data property
                        addDataPropertyToIndividual(config, rQual, actionIndividual, hasDuration, duration, XSDDatatype.XSDstring)
                    }.getOrElse(Right(rQual)) match {
                      case Right(rDuration) =>
                        a.reference.map {
                          reference =>
                            Right(
                              rDuration.copy(
                                // Prepend new reference that needs to be created
                                connectionReferences = ConnectionActionContainsAction(actionIndividual.getURI, reference.name) :: rDuration.connectionReferences
                              )
                            )
                        }.getOrElse(Left(OntoPlcSemanticLiftingError(s"Could not retrieve reference from action [$a].")))
                      case l@Left(_) => l
                    }
                  case l@Left(_) => l
                }
              case l@Left(_) => l
            }
          case l@Left(_) => l
        }
      }

      def retrieveConnectionPointIn(r4: OntoPlcSemanticParsingResult, individual: Individual, r: DataRecord[_], constructor: (String, String) => ConnectionPointReference): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
        r.value match {
          case c: Connection =>
            Right(
              r4.copy(
                connectionReferences =
                // Prepend new reference that needs to be created
                  constructor(c.refLocalId.toString, individual.getURI) :: r4.connectionReferences
              )
            )
          case _ => Left(OntoPlcSemanticLiftingError(s"Could not match connection point in [${
            r.value
          }]."))
        }
      }

      def processConnectionPointInsForConnectedToReference(r6: OntoPlcSemanticParsingResult, individual: Individual, connectionPointInList: List[DataRecord[Any]]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
        LazyList.unfold((r6, connectionPointInList)) {
          case (res, recs) =>
            recs.headOption.map {
              rec =>
                retrieveConnectionPointIn(res, individual, rec, new ConnectionTo(_, _)).fold(Left(_) -> (res, Nil), r => Right(r) -> (r, recs.tail))
            }
        }.last

      // Here, we directly match on the data record's value
      r.value match {
        case s: Step =>
          // Create step individual
          val stepClass = r2.model.getOntClass(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#Step")
          val stepIndividual = r2.model.createIndividual(getStepIndividualUri(config, s.name, s.localId.toString()), stepClass)
          // Add 'hasStep' object property
          val hasStepProperty = r2.model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#hasStep")
          individual.addProperty(hasStepProperty, stepIndividual)
          // Add required attributes 'hasName' and 'hasLocalId'
          val resultHasLocalId = for {
            rHasName <- addDataPropertyToIndividual(config, r2, stepIndividual, hasName, s.name, XSDDatatype.XSDstring)
            rHasLocalId <- addDataPropertyToIndividual(config, rHasName, stepIndividual, hasLocalId, s.localId.toString(), XSDDatatype.XSDinteger)
          } yield rHasLocalId
          resultHasLocalId match {
            case Right(rHasLocalId) =>
              // Global ID is optional
              s.globalId.map {
                globalId =>
                  // Add 'hasGlobalId' data property
                  addDataPropertyToIndividual(config, rHasLocalId, stepIndividual, hasGlobalId, globalId, XSDDatatype.XSDstring)
              }.getOrElse(Right(rHasLocalId)) match {
                case Right(rHasGlobalId) =>
                  // Initial step
                  Option.when(s.initialStep)(true).map {
                    initialStep =>
                      // Add 'isInitialStep' data property
                      addDataPropertyToIndividual(config, rHasGlobalId, stepIndividual, isInitialStep, initialStep.toString, XSDDatatype.XSDboolean)
                  }.getOrElse(Right(rHasLocalId)) match {
                    case Right(rInitialStep) =>
                      s.connectionPointIn.map {
                        connIn =>
                          processConnectionPointInsForConnectedToReference(rInitialStep, stepIndividual, connIn.connectionpointinableoption.toList)
                      }.getOrElse(Right(rInitialStep))
                    case l@Left(_) => l
                  }
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case t: Transition2 =>
          // Create transition individual
          val transitionClass = r2.model.getOntClass(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#Transition")
          val transitionIndividual = r2.model.createIndividual(getTransitionIndividualUri(config, t.localId.toString()), transitionClass)
          // Add 'hasTransition' object property
          val hasTransitionProperty = r2.model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#hasTransition")
          individual.addProperty(hasTransitionProperty, transitionIndividual)
          // Add 'hasLocalId' data property
          addDataPropertyToIndividual(config, r2, transitionIndividual, hasLocalId, t.localId.toString(), XSDDatatype.XSDinteger) match {
            case Right(rHasLocalId) =>
              t.globalId.map {
                globalId =>
                  // Add 'hasGlobalId' data property
                  addDataPropertyToIndividual(config, rHasLocalId, transitionIndividual, hasGlobalId, globalId, XSDDatatype.XSDstring)
              }.getOrElse(Right(rHasLocalId)) match {
                case Right(rHasGlobalId) =>
                  // Add reference in condition element
                  t.condition.map {
                    condition =>
                      condition.conditionoption.value match {
                        case Inline(bo, _, _, _) =>
                          bo.key.map {
                            case "ST" => createStructuredTextStatement(rHasGlobalId, transitionIndividual, bo)
                            case _ => Left(OntoPlcSemanticLiftingError("Could not match bodyable option key."))
                          }.getOrElse(Right(rHasGlobalId))
                        case Reference2(attributes) =>
                          attributes.get("@name").map {
                            r =>
                              val refName = r.value.toString
                              // Prepend new reference that needs to be created
                              Right(
                                rHasGlobalId.copy(
                                  connectionReferences = ConnectionTransitionContainsTransition(transitionIndividual.getURI, refName) :: rHasGlobalId.connectionReferences
                                )
                              )
                          }.getOrElse(Left(OntoPlcSemanticLiftingError(s"Could not retrieve name of referenced element [$attributes].")))
                        case _ => Left(OntoPlcSemanticLiftingError(s"Could not match condition child [${
                          condition.conditionoption.value
                        }]."))
                      }
                  }.getOrElse(Right(rHasGlobalId)) match {
                    // Create connections
                    case Right(rConditionReference) =>
                      t.connectionPointIn.map {
                        connIn =>
                          processConnectionPointInsForConnectedToReference(rConditionReference, transitionIndividual, connIn.connectionpointinableoption.toList)
                      }.getOrElse(Right(rConditionReference))
                    case l@Left(_) => l
                  }
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case ab: ActionBlock =>
          // Create 'ActionBlock' individual
          val actionBlockClass = r2.model.getOntClass(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#ActionBlock")
          val actionBlockIndividual = r2.model.createIndividual(getActionBlockIndividualUri(config, ab.localId.toString()), actionBlockClass)
          // Add 'hasLocalId' data property
          addDataPropertyToIndividual(config, r2, actionBlockIndividual, hasLocalId, ab.localId.toString(), XSDDatatype.XSDinteger) match {
            case Right(rHasLocalId) =>
              // Execution Order ID is optional
              ab.executionOrderId.map {
                id =>
                  // Add 'hasExecutionOrderId' data property
                  addDataPropertyToIndividual(config, rHasLocalId, actionBlockIndividual, hasExecutionOrderId, id.toString(), XSDDatatype.XSDinteger)
              }.getOrElse(Right(rHasLocalId)) match {
                case Right(rExOrId) =>
                  // Retrieve connection points to associate steps to action blocks afterwards
                  ab.connectionPointIn.map {
                    connIn =>
                      LazyList.unfold((rExOrId, connIn.connectionpointinableoption.toList)) {
                        case (res, recs) =>
                          recs.headOption.map {
                            rec =>
                              retrieveConnectionPointIn(res, actionBlockIndividual, rec, new ConnectionStepToActionBlock(_, _)).fold(Left(_) -> (res, Nil), r => Right(r) -> (r, recs.tail))
                          }
                      }.last
                  }.getOrElse(Right(rExOrId)) match {
                    case Right(rConnPointIn) =>
                      LazyList.unfold((rConnPointIn, ab.action.toList)) {
                        case (res, recs) =>
                          recs.headOption.map {
                            rec =>
                              createAction(res, actionBlockIndividual, rec).fold(Left(_) -> (res, Nil), r => Right(r) -> (r, recs.tail))
                          }
                      }.last
                    case l@Left(_) => l
                  }
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case simDiv: SimultaneousDivergence =>
          // Create 'SimultaneousDivergence' individual
          val simultaneousDivergenceClass = r2.model.getOntClass(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#SimultaneousDivergence")
          val simultaneousDivergenceIndividual = r2.model.createIndividual(getSimultaneousDivergenceIndividualUri(config, simDiv.localId.toString()), simultaneousDivergenceClass)
          // Add 'hasSimultaneousDivergence' object property
          val hasSimultaneousDivergenceProperty = r2.model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#hasSimultaneousDivergence")
          individual.addProperty(hasSimultaneousDivergenceProperty, simultaneousDivergenceIndividual)
          // Add 'hasLocalId' data property
          addDataPropertyToIndividual(config, r2, simultaneousDivergenceIndividual, hasLocalId, simDiv.localId.toString(), XSDDatatype.XSDinteger) match {
            case Right(rHasLocalId) =>
              simDiv.globalId.map {
                globalId =>
                  // Add 'hasGlobalId' data property
                  addDataPropertyToIndividual(config, rHasLocalId, simultaneousDivergenceIndividual, hasGlobalId, globalId, XSDDatatype.XSDstring)
              }.getOrElse(Right(rHasLocalId)) match {
                case Right(rHasGlobalId) =>
                  simDiv.connectionPointIn.map {
                    connIn =>
                      processConnectionPointInsForConnectedToReference(rHasGlobalId, simultaneousDivergenceIndividual, connIn.connectionpointinableoption.toList)
                  }.getOrElse(Right(rHasGlobalId))
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case simConv: SimultaneousConvergence =>
          // Create 'SimultaneousConvergence' individual
          val simultaneousConvergenceClass = r2.model.getOntClass(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#SimultaneousConvergence")
          val simultaneousConvergenceIndividual = r2.model.createIndividual(getSimultaneousConvergenceIndividualUri(config, simConv.localId.toString()), simultaneousConvergenceClass)
          // Add 'hasSimultaneousConvergence' object property
          val hasSimultaneousConvergenceProperty = r2.model.getObjectProperty(s"${
            config.sfcConfig.ontoPlcConfig.ns
          }#hasSimultaneousConvergence")
          individual.addProperty(hasSimultaneousConvergenceProperty, simultaneousConvergenceIndividual)
          // Add 'hasLocalId' data property
          addDataPropertyToIndividual(config, r2, simultaneousConvergenceIndividual, hasLocalId, simConv.localId.toString(), XSDDatatype.XSDinteger) match {
            case Right(rHasLocalId) =>
              simConv.globalId.map {
                globalId =>
                  // Add 'hasGlobalId' data property
                  addDataPropertyToIndividual(config, rHasLocalId, simultaneousConvergenceIndividual, hasGlobalId, globalId, XSDDatatype.XSDstring)
              }.getOrElse(Right(rHasLocalId)) match {
                case Right(rHasGlobalId) =>
                  val connectionPointInList = simConv.connectionPointIn.toList.flatMap(_.connectionpointinableoption)
                  processConnectionPointInsForConnectedToReference(rHasGlobalId, simultaneousConvergenceIndividual, connectionPointInList)
                case l@Left(_) => l
              }
            case l@Left(_) => l
          }
        case macroStep: MacroStep => throw new NotImplementedError()
        case jumpStep: JumpStep =>
          logger.error(s"Jump steps are not yet implemented. Affecting: $jumpStep.")
          Right(r2) // Important: Return here the correct parsing result, otherwise parts of the model will be lost!
        case selectDiv: SelectionDivergence => throw new NotImplementedError()
        case selectConv: SelectionConvergence => throw new NotImplementedError()
        case _ => Left(OntoPlcSemanticLiftingError(s"Could not match element of SFC's body [${
          r.value
        }]."))
      }
    }


    def createStructuredTextStatement(res: OntoPlcSemanticParsingResult, inv: Individual, bo: DataRecord[BodyableOption]): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
      bo.value match {
        case FormattedText(fmt) =>
          val fmtKV = (fmt.key, fmt.value)
          fmtKV match {
            case (Some(k), v: Elem) if k == "p" && v.text.trim.nonEmpty =>
              val stStatementClass = res.model.getOntClass(s"${
                config.sfcConfig.ontoPlcConfig.ns
              }#StStatement")
              // Create 'StStatement' individual
              val stStatementIndividual = res.model.createIndividual(getStStatementIndividualUri(config, inv), stStatementClass)
              // Add 'contains' object property
              val containsObjectProperty = res.model.getObjectProperty(s"${
                config.sfcConfig.ontoPlcConfig.ns
              }#contains")
              inv.addProperty(containsObjectProperty, stStatementIndividual)
              // Add 'hasStatement' data property
              addDataPropertyToIndividual(config, res, stStatementIndividual, "hasStatement", v.text.trim, XSDDatatype.XSDstring)
            case _ => Right(res)
          }
        case _ => Left(OntoPlcSemanticLiftingError("Could not match bodyable option value."))
      }
    }

    try {
      bodyableOption.key.map {
        case "ST" => createStructuredTextStatement(prevParsingResult, individual, bodyableOption)
        case "SFC" =>
          // Process SFC body
          bodyableOption.value match {
            case SFC2(sfc2option) =>
              LazyList.unfold((prevParsingResult, sfc2option.toList)) {
                case (res, recs) =>
                  recs.headOption.map {
                    rec =>
                      createSfcElement(res, rec).fold(Left(_) -> (res, Nil), r => Right(r) -> (r, recs.tail))
                  }
              }.last
            case _ => Left(OntoPlcSemanticLiftingError("Could not match SFC element in body."))
          }
        case _ => Left(OntoPlcSemanticLiftingError("Could not match bodyable option key."))
      }.getOrElse(Right(prevParsingResult))
    } catch {
      case e: Throwable =>
        val err = OntoPlcSemanticLiftingError(s"Could not add body [$bodyableOption] to individual [$individual].")
        logger.error(err.message, e)
        Left(err)
    }
  }

  private def addDataPropertyToIndividual(
                                           config: Config,
                                           prevParsingResult: OntoPlcSemanticParsingResult,
                                           individual: Individual,
                                           propName: String,
                                           value: String,
                                           propType: RDFDatatype
                                         ): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] =
    try {
      val ontModel = prevParsingResult.model
      val property = ontModel.getDatatypeProperty(s"${
        config.sfcConfig.ontoPlcConfig.ns
      }#$propName")
      individual.addProperty(property, ResourceFactory.createTypedLiteral(value, propType))
      Right(prevParsingResult.copy(model = ontModel))
    } catch {
      case e: Throwable =>
        val err = OntoPlcSemanticLiftingError(s"Could not add data property [$propName=$value].")
        logger.error(err.message, e)
        Left(err)
    }

  private def getProjectIndividualUri(config: Config, projectName: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#project_$projectName"

  private def getStStatementIndividualUri(config: Config, parentIndividual: Individual): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#st_${
      OntModelUtils.removeNamespace(parentIndividual.getURI)
    }"

  private def getActionIndividualUri(config: Config, actionNameOrGlobalId: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#action_$actionNameOrGlobalId"

  private def getStepIndividualUri(config: Config, stepName: String, localId: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#step_${
      stepName
    }_${
      localId
    }"

  private def getTransitionIndividualUri(config: Config, transitionNameOrLocalId: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#transition_$transitionNameOrLocalId"

  private def getPouIndividualUri(config: Config, pouType: String, pouName: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#${
      pouType
    }_$pouName"

  private def getSimultaneousDivergenceIndividualUri(config: Config, localId: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#simultaneous_divergence_$localId"

  private def getSimultaneousConvergenceIndividualUri(config: Config, localId: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#simultaneous_convergence_$localId"

  private def getActionBlockIndividualUri(config: Config, localId: String): String =
    s"${
      config.sfcConfig.sfcTransformationOntConfig.ns
    }#action_block_$localId"

  def apply(config: Config, project: Project): Either[OntoPlcSemanticLiftingError, OntoPlcSemanticParsingResult] = {
    val ontModel = createOntModel(config)
    for {
      rPrjMdt <- createProjectMetaData(config, OntoPlcSemanticParsingResult(model = ontModel), project)
      rPous <- createProgramOrganizationUnits(config, rPrjMdt, project, project.types.pous.pou.toList)
      rRefs <- createReferences(config, rPous)
    } yield rRefs

  }

}
