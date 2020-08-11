package org.sba_research.utils

import com.typesafe.scalalogging.Logger
import org.apache.jena.query._
import org.apache.jena.rdf.model.Model
import org.apache.jena.sparql.core.Var
import org.apache.jena.sparql.engine.binding.Binding

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

case class ResultSetBinding(variables: List[Var], values: List[Binding])

object QueryExecutor {

  val logger = Logger(getClass)

  def query(s: String, service: String): Unit = {
    val q = QueryFactory.create(s)
    val qexec: QueryExecution = QueryExecutionFactory.sparqlService(service, q)
    val result: Try[ResultSet] = Try(qexec.execSelect())
    result match {
      case Success(r) =>
        ResultSetFormatter.out(System.out, r, q)
      case Failure(t) => logger.error(s"Failed to execute query: $t")
    }
    qexec.close()
  }

  @tailrec
  private def getValuesFromResultSet(r: ResultSet, l: List[Binding] = List.empty): List[Binding] =
    if (r.hasNext) getValuesFromResultSet(r, l ::: List(r.nextBinding()))
    else l

  def query(s: String, model: Model, resBinding: Option[ResultSetBinding]): ResultSetBinding = {
    val q = QueryFactory.create(s)
    resBinding.foreach(r => q.setValuesDataBlock(r.variables.asJava, r.values.asJava))
    val qexec = QueryExecutionFactory.create(q, model)
    executeSelectQuery(q, qexec)
  }

  def query(s: String, service: String, resBinding: ResultSetBinding): ResultSetBinding = {
    val q = QueryFactory.create(s)
    q.setValuesDataBlock(resBinding.variables.asJava, resBinding.values.asJava)
    val qexec: QueryExecution = QueryExecutionFactory.sparqlService(service, q)
    // Important! 'VALUES' block will not be considered in execution, when setting it after qexec!
    // qexec.getQuery.setValuesDataBlock(resBinding.variables.asJava, resBinding.values.asJava)
    logger.debug(qexec.getQuery.toString)
    executeSelectQuery(q, qexec)
  }

  private def executeSelectQuery(q: Query, qexec: QueryExecution): ResultSetBinding = {
    val result: Try[ResultSet] = Try(qexec.execSelect())
    val ret = result match {
      case Success(r) =>
        val results = ResultSetFactory.copyResults(r)
        ResultSetFormatter.out(System.out, results, q)
        results.reset()
        val variables = results.getResultVars.asScala.toList.map(Var.alloc)
        val values = getValuesFromResultSet(results)
        ResultSetBinding(variables, values)
      case Failure(t) =>
        logger.error(s"Failed to execute query: $t")
        ResultSetBinding(List.empty, List.empty)
    }
    qexec.close()
    ret
  }

  def construct(s: String, model: Model, reasonerUri: Option[String]): Option[Model] = {
    val q = QueryFactory.create(s)
    val m: Model = reasonerUri.map(OntModelUtils.getInfModel(model, _)).getOrElse(model)
    val qexec = QueryExecutionFactory.create(q, m)

    executeConstructQuery(q, qexec)
  }

  private def executeConstructQuery(q: Query, qexec: QueryExecution): Option[Model] = {
    val result: Try[Model] = Try(qexec.execConstruct())
    val ret = result match {
      case Success(m) => Some(m)
      case Failure(t) =>
        logger.error(s"Failed to execute query: $t")
        None
    }
    qexec.close()
    ret
  }

}
