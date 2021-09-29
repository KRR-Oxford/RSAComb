/*
 * Copyright 2020, 2021 KRR Oxford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ox.cs.rsacomb.util

import java.io.{OutputStream, File, StringReader}
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.{
  ComponentInfo,
  ConnectionFactory,
  ServerConnection,
  DataStoreConnection,
  UpdateType
}
import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  Rule,
  BodyFormula,
  Negation,
  TupleTableAtom,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Literal,
  Resource,
  Variable,
  Term
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.suffix.Nth
import uk.ac.ox.cs.rsacomb.util.Logger

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
  private type QueryAnswers = Seq[(Long, Seq[Resource])]
  private def QueryAnswers() = List.empty[(Long, Seq[Resource])]

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
    * @see [[uk.ac.ox.cs.rsacomb.util.RDFoxUtil.closeConnection RDFoxUtil.closeConnection]]
    * for details on how to close an open connection.
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
    opts.put("type", "par-complex-nn")
    if (!server.containsDataStore(datastore))
      server.createDataStore(datastore, opts)
    val data = server.newDataStoreConnection(datastore)
    (server, data)
  }

  /** Create a built-in `rdfox:SKOLEM` TupleTableAtom. */
  def skolem(name: String, terms: Term*): TupleTableAtom =
    TupleTableAtom.create(
      TupleTableName.SKOLEM,
      (Literal.create(name, Datatype.XSD_STRING) +: terms): _*
    )

  /** Prints statistics from RDFox datastore.
    *
    * Prints something only when Logger level is set to DEBUG or more.
    *
    * @see [[https://docs.oxfordsemantic.tech/programmatic-access-APIs.html#in-depth-diagnostic-information]]
    * and [[https://docs.oxfordsemantic.tech/programmatic-access-APIs.html#managing-statistics]]
    * for more ways of gathering diagnostics from RDFox.
    */
  def printStatisticsFor(data: DataStoreConnection): Unit = {
    val info = data.getComponentInfo(true)
    val stats = s"${info.getName}: ${info.getPropertyValues}"
      .replaceAll("\\{", "{\n  ")
      .replaceAll(", ", ",\n  ")
      .replaceAll("\\}", "\n}")
    Logger.print(stats, Logger.DEBUG)
  }

  /** Adds a collection of rules to a data store.
    *
    * @param data datastore connection
    * @param rules collection of rules to be added to the data store
    */
  def addRules(data: DataStoreConnection, rules: Seq[Rule]): Unit =
    Logger.timed(
      if (rules.length > 0) {
        data.importData(
          UpdateType.ADDITION,
          RSA.Prefixes,
          rules
            .map(_.toString(Prefixes.s_emptyPrefixes))
            .mkString("\n")
        )
      },
      s"Loading ${rules.length} rules",
      Logger.DEBUG
    )

  /** Adds a collection of facts to a data store.
    *
    * @param data datastore connection
    * @param facts collection of facts to be added to the data store
    */
  def addFacts(data: DataStoreConnection, facts: Seq[TupleTableAtom]): Unit =
    Logger.timed(
      if (facts.length > 0) {
        data.importData(
          UpdateType.ADDITION,
          RSA.Prefixes,
          facts
            .map(_.toString(Prefixes.s_emptyPrefixes))
            .mkString("", ".\n", ".")
        )
      },
      s"Loading ${facts.length} facts",
      Logger.DEBUG
    )

  /** Imports a sequence of files directly into a datastore.
    *
    * @param data datastore connection.
    * @param files sequence of files to upload.
    */
  def addData(data: DataStoreConnection, files: File*): Unit =
    Logger.timed(
      files.foreach {
        data.importData(
          UpdateType.ADDITION,
          RSA.Prefixes,
          _
        )
      },
      "Loading data files",
      Logger.DEBUG
    )

  /** Force materialization in RDFox. */
  def materialize(data: DataStoreConnection): Unit =
    Logger.timed(data.updateMaterialization(), "Materialization", Logger.DEBUG)

  /** Export data in `text/turtle`.
    *
    * @param data datastore connection from which to export data.
    * @param rules output stream for rules
    * @param facts output stream for facts
    */
  def export(
      data: DataStoreConnection,
      rules: OutputStream,
      facts: OutputStream
  ): Unit = {
    data.exportData(Prefixes.s_emptyPrefixes, facts, "text/turtle", RDFoxOpts())
    data.exportData(
      Prefixes.s_emptyPrefixes,
      rules,
      "application/x.datalog",
      RDFoxOpts()
    )
  }

  /** Load SPARQL queries from file.
    *
    * The file can list multiple queries, each preceeded with a
    * single line containing "#^[Query<id>]" where "<id>" is a number.
    * Empty lines are ignored.
    *
    * @note if a query is not recognized as a [[SelectQuery]] by RDFox
    * it is discarded.
    *
    * @param file file containing a list of conjunctive queries.
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    *
    * @return a list of [[tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery SelectQuery]] queries.
    */
  def loadQueriesFromFile(
      file: File,
      prefixes: Prefixes = new Prefixes()
  ): List[ConjunctiveQuery] = {
    val source = io.Source.fromFile(file)
    val queries = source.getLines
      .map(_.trim.filter(_ >= ' '))
      .filterNot(_ == "")
      .foldRight((List.empty[List[String]], List.empty[String])) {
        case (line, (acc, query)) => {
          if ("^#\\^\\[Query\\d+\\]$".r.matches(line))
            (query :: acc, List.empty)
          else
            (acc, line :: query)
        }
      }
      ._1
      .map(_.mkString(" "))
      .map(ConjunctiveQuery.parse(_, prefixes))
      .collect { case Some(q) => q }
    Logger print s"Loaded ${queries.length} queries from ${file.getAbsolutePath}"
    source.close()
    queries
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
  ): QueryAnswers = Logger.timed(
    {
      val cursor = data.createCursor(query, opts)
      var answers = QueryAnswers()
      var mul = cursor.open()
      while (mul > 0) {
        val answer =
          (0 until cursor.getArity).map(cursor.getResource(_)).toList
        answers = (mul, answer) :: answers
        mul = cursor.advance()
      }
      cursor.close();
      answers
    },
    "Answer query",
    Logger.DEBUG
  )

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
      val variables = (0 until arity).mkString("?X", " ?X", "")
      s"""
      SELECT $variables
      WHERE {
          ?K a rsa:$pred.
          TT <http://oxfordsemantic.tech/RDFox#SKOLEM> { $variables ?K } .
      }
      """
    } else {
      "ASK { ?X a rsa:Ans }"
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
    val (sk, as) = rule.getHead.map(_.reified).unzip
    val head: List[TupleTableAtom] = as.flatten
    val skolem: List[BodyFormula] = sk.flatten
    val body: List[BodyFormula] = rule.getBody.map(reify).flatten
    Rule.create(head, skolem ::: body)
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
        val (sk, as) = neg.getNegatedAtoms
          .map({
            case a: TupleTableAtom => a.reified
            case a                 => (None, List(a))
          })
          .unzip
        val skolem =
          sk.flatten.map(_.getArguments.last).collect { case v: Variable => v }
        val atoms = as.flatten
        List(Negation.create(skolem, atoms))
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
