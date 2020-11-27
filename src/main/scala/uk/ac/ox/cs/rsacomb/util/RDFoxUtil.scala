package uk.ac.ox.cs.rsacomb.util

import java.io.StringReader
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.{
  ConnectionFactory,
  ServerConnection,
  DataStoreConnection
}
import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Rule,
  BodyFormula,
  Negation,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{Resource}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import uk.ac.ox.cs.rsacomb.suffix.Nth

/** A collection of helper methods for RDFox */
object RDFoxUtil {

  /** Simplify conversion between Java and Scala `List`s */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** Extends capabilities of
    *  [[tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom TupleTableAtom]].
    */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAtom._

  /** Type alias for a collection of answers to a
    * [[tech.oxfordsemantic.jrdfox.logic.sparql.statement.Query]].
    */
  private type QueryAnswers = Seq[Seq[Resource]]
  private def QueryAnswers() = List.empty[Seq[Resource]]

  /** Type alias for <option => value> RDFox options. */
  private type RDFoxOpts = java.util.Map[String, String]
  private def RDFoxOpts() = new java.util.HashMap[String, String]()

  /** Setup a new local connection with RDFox.
    *
    * @param dataStore data store identifier
    * @param opts additional options to RDFox
    * @return a tuple with the newly opened server and data store
    * connections.
    *
    * @see [[uk.ac.ox.cs.rsacomb.util.RDFoxUtil.closeConnection
    * RDFoxUtil.closeConnection]] for
    * details on how to close an open connection.
    */
  def openConnection(
      datastore: String,
      opts: RDFoxOpts = RDFoxOpts()
  ): (ServerConnection, DataStoreConnection) = {
    val serverUrl = "rdfox:local"
    val role = ""
    val password = ""
    val server =
      ConnectionFactory.newServerConnection(serverUrl, role, password)
    if (!server.containsDataStore(datastore))
      server.createDataStore(datastore, "par-complex-nn", opts)
    val data = server.newDataStoreConnection(datastore)
    (server, data)
  }

  /** Parse a SELECT query from a string in SPARQL format.
    *
    * @param query the string containing the SPARQL query
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    * @return a [[tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery SelectQuery]]
    * if the input string is a SELECT query, none otherwise.
    */
  def parseSelectQuery(
      query: String,
      prefixes: Prefixes = new Prefixes()
  ): Option[SelectQuery] = {
    val parser = new SPARQLParser(
      prefixes,
      new StringReader(query)
    )
    parser.parseSingleQuery() match {
      case q: SelectQuery => Some(q)
      case _              => None
    }
  }

  /** Execute a query over a given datastore connection.
    *
    * @param data RDFox datastore connection.
    * @param query a
    * [[tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery SelectQuery]]
    * to be executed.
    * @param opts additional options to RDFox.
    * @returns a collection of answers to the query.
    */
  def submitSelectQuery(
      data: DataStoreConnection,
      query: SelectQuery,
      opts: RDFoxOpts = RDFoxOpts()
  ): QueryAnswers = {
    val cursor = data.createCursor(query, opts)
    var answers = QueryAnswers()
    var mul = cursor.open()
    while (mul > 0) {
      val answer =
        (0 until cursor.getArity).map(cursor.getResource(_)).toList
      answers = answer :: answers
      mul = cursor.advance()
    }
    cursor.close();
    answers
  }

  /** Execute a query over a given datastore connection.
    *
    * @param data RDFox datastore connection.
    * @param query a string representing a SPARQL query.
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    * @param opts additional options to RDFox.
    * @returns a collection of answers to the query if the input query
    * is a SELECT query, none otherwise.
    */
  def submitQuery(
      data: DataStoreConnection,
      query: String,
      prefixes: Prefixes = new Prefixes(),
      opts: RDFoxOpts = RDFoxOpts()
  ): Option[QueryAnswers] =
    parseSelectQuery(query, prefixes).map(submitSelectQuery(data, _, opts))

  /** Returns a query describing an internal predicate.
    *
    * In the RSA combined approach internal predicates are reified to be
    * compatible with RDFox engine. This helper allows to build a query
    * to gather all instances of an internal predicate
    *
    * @param pred name of the predicate to describe.
    * @param arity arity of the predicate.
    * @return a string containing a SPARQL query.
    */
  def buildDescriptionQuery(
      pred: String,
      arity: Int
  ): String = {
    if (arity > 0) {
      (0 until arity).mkString("SELECT ?X", " ?X", "\n") +
        (0 until arity)
          .map(i => s"?S rsa:${pred :: Nth(i)} ?X$i .")
          .mkString("WHERE {\n", "\n", "\n}")
    } else {
      s"ASK { ?X a rsa:$pred }"
    }
  }

  /** Reify a [[tech.oxfordsemantic.jrdfox.logic.datalog.Rule Rule]].
    *
    * This is needed because RDFox supports only predicates of arity 1
    * or 2, but the filtering program uses predicates with higher arity.
    *
    * @note we can perform a reification of the atoms thanks to the
    * built-in `SKOLEM` funtion of RDFox.
    */
  def reify(rule: Rule): Rule = {
    val (bs, as) = rule.getHead.map(_.reified).unzip
    val head: List[TupleTableAtom] = as.flatten
    val bind: List[BodyFormula] = bs.flatten
    val body: List[BodyFormula] = rule.getBody.map(reify).flatten
    Rule.create(head, bind ::: body)
  }

  /** Reify a [[tech.oxfordsemantic.jrdfox.logic.datalog.BodyFormula BodyFormula]].
    *
    * This is needed because RDFox supports only predicates of arity 1
    * or 2, but the filtering program uses predicates with higher arity.
    *
    * @note we can perform a reification of the atoms thanks to the
    * built-in `SKOLEM` funtion of RDFox.
    */
  private def reify(formula: BodyFormula): List[BodyFormula] = {
    formula match {
      case atom: TupleTableAtom => atom.reified._2
      case neg: Negation => {
        val (bs, as) = neg.getNegatedAtoms
          .map({
            case a: TupleTableAtom => a.reified
            case a                 => (None, List(a))
          })
          .unzip
        val bind = bs.flatten.map(_.getBoundVariable)
        val atoms = as.flatten
        List(Negation.create(bind, atoms))
      }
      case other => List(other)
    }
  }

  /** Close an open connection to RDFox.
    *
    * @param server server connection
    * @param data data store connections
    *
    * @see [[uk.ac.ox.cs.rsacomb.util.RDFoxUtil.openConnection RDFoxUtil.openConnection]]
    * for details on how to create a new connection with RDFox.
    */
  def closeConnection(
      server: ServerConnection,
      data: DataStoreConnection
  ): Unit = {
    data.close();
    server.close();
  }

}