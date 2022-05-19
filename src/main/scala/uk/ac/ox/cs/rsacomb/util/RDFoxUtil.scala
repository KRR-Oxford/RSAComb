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
import scala.collection.JavaConverters._
import org.semanticweb.owlapi.model.OWLLogicalAxiom
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
  Term,
  IRI
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery
import uk.ac.ox.cs.rsacomb.sparql.ConjunctiveQuery
import uk.ac.ox.cs.rsacomb.suffix.Nth
import uk.ac.ox.cs.rsacomb.util.Logger

object RDFoxDSL {

  import scala.collection.JavaConverters._
  import tech.oxfordsemantic.jrdfox.logic.datalog._
  //import tech.oxfordsemantic.jrdfox.logic.expression._


  /* String contexts */
  implicit class MyVariable(private val str: StringContext) extends AnyVal {
    def v(args: Any*): Variable = Variable.create(s"${str.s(args: _*)}")
  }
  implicit class MyIRI(private val str: StringContext) extends AnyVal {
    def i(args: Any*): IRI = IRI.create(s"${str.s(args: _*)}")
  }


  /** Sequence of [[TupleTableAtom]]s (a.k.a. head of a rule) */
  case class SeqTupleTableAtoms(atoms: TupleTableAtom*) {
    /** Atoms conjunction */
    def +(atom: TupleTableAtom): SeqTupleTableAtoms =
      SeqTupleTableAtoms(atoms :+ atom: _*)
    /** Implication (rule constructor) */
    def :-(body: SeqBodyFormulas): Rule = {
      Rule.create(atoms.asJava, body.atoms.asJava)
    }
  }
  implicit def seqTupleTableAtoms(atom: TupleTableAtom): SeqTupleTableAtoms =
    SeqTupleTableAtoms(atom)

  /** Sequence of [[BodyFormula]]e (a.k.a. body) */
  case class SeqBodyFormulas(atoms: BodyFormula*) {
    /** Atoms conjunction */
    def +(atom: BodyFormula): SeqBodyFormulas = SeqBodyFormulas(atoms :+ atom: _*)
  }
  implicit def seqBodyFormulas(atom: BodyFormula): SeqBodyFormulas =
    SeqBodyFormulas(atom)
  implicit def atoms2bodyFormulas(ttas: SeqTupleTableAtoms): SeqBodyFormulas =
    SeqBodyFormulas(ttas.atoms: _*)




  trait RDFoxPredicate {
    def apply(terms: Term*): TupleTableAtom
  }

  implicit class RDFoxGraph(val tt: IRI) extends RDFoxPredicate {
    def apply(terms: Term*): TupleTableAtom =
      TupleTableAtom.create(TupleTableName.create(tt.getIRI), terms:_*)
  }
  //class RDFoxTerm() extends RDFoxPredicate {}

  //implicit class RDFoxPrefix(val name: String) {
  //  def ::(prefix: String): IRI = IRI.create(prefix + name)
  //}

  //val owlpre = "http://example.com/owl#"
  //val prova = TupleTableAtom.rdf(v"x", v"y", v"z") & TupleTableAtom.rdf(v"x", v"y", v"z")
  //val prova2 = i"iri_di_prova"(v"x", v"y", v"z") & owlpre::"subClassOf"(v"x", v"y", v"z")

}

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
  def QueryAnswers() = List.empty[(Long, Seq[Resource])]

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

  /** Get the IRI of a named graph (creating it if necessary)
    *
    * @param datastore name of the datastore to perform the action in.
    * @param name name of the named graph.
    *
    * @return the full IRI for the (new) named graph.
    */
  def getNamedGraph(datastore: String, name: String): IRI = {
    val graph = RSA(name)
    val (server, data) = openConnection(datastore)
    if (!data.containsTupleTable(graph.getIRI))
      data.createTupleTable(graph.getIRI, Map("type" -> "named-graph").asJava)
    RDFoxUtil.closeConnection(server, data)
    return graph
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
        data addRules rules
        // data.importData(
        //   UpdateType.ADDITION,
        //   RSA.Prefixes,
        //   rules
        //     .map(_.toString(Prefixes.s_emptyPrefixes))
        //     .mkString("\n")
        // )
      },
      s"Loading ${rules.length} rules",
      Logger.DEBUG
    )

  /** Adds a collection of facts to a data store.
    *
    * @param data datastore connection
    * @param facts collection of facts to be added to the data store
    */
  def addFacts(
      data: DataStoreConnection,
      graph: IRI,
      facts: Seq[TupleTableAtom]
  ): Unit =
    Logger.timed(
      if (facts.length > 0) {
        data.importData(
          graph.getIRI,
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

  /** Adds a collection of facts to a data store.
    *
    * @param data datastore connection
    * @param facts collection of facts to be added to the data store
    */
  def addAxioms(
      data: DataStoreConnection,
      gsource: IRI,
      gtarget: IRI,
      axioms: Seq[OWLLogicalAxiom]
  ): Unit =
    Logger.timed(
      if (axioms.length > 0) {
        data.importData(
          gsource.getIRI,
          UpdateType.ADDITION,
          RSA.Prefixes,
          axioms
            .map(_.toString)
            .mkString("Ontology(\n","\n",")")
        )
        data.importAxiomsFromTriples(gsource.getIRI, true, gtarget.getIRI, UpdateType.ADDITION)
      },
      s"Loading ${axioms.length} axioms",
      Logger.DEBUG
    )

  /** Imports a sequence of files directly into a datastore.
    *
    * @param data datastore connection.
    * @param graph named graph where the data should be uploaded.
    * @param files sequence of files to upload.
    */
  def addData(data: DataStoreConnection, graph: IRI, files: os.Path*): Unit =
    Logger.timed(
      files.foreach { path =>
        data.importData(
          graph.getIRI,
          UpdateType.ADDITION,
          RSA.Prefixes,
          path.toIO
        )
      },
      "Loading data files",
      Logger.DEBUG
    )

  /** Execute an update SPARQL query on a datastore.
    *
    * @param data datastore connection.
    * @param query update SPARQL query.
    */
  def updateData(data: DataStoreConnection, query: String): Unit =
    Logger.timed(
      data.evaluateUpdate(
        null, // the base IRI for the query (if null, a default is used)
        RSA.Prefixes,
        query,
        new java.util.HashMap[String, String]
      ),
      "Updating data",
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
    * @param path file containing a list of conjunctive queries.
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    *
    * @return a list of [[tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery SelectQuery]] queries.
    */
  def loadQueriesFromFile(
      path: os.Path,
      prefixes: Prefixes = new Prefixes()
  ): List[ConjunctiveQuery] = {
    val header = "#?\\^\\[[Qq]uery(\\d+)\\]".r
    val comment = "^#.*".r
    val queries = os.read
      .lines(path)
      .map(_.trim.filter(_ >= ' '))
      .filterNot(_ == "")
      .foldRight((List.empty[Option[ConjunctiveQuery]], List.empty[String])) {
        case (line, (acc, query)) => {
          line match {
            case header(id) => {
              if (query.isEmpty) {
                (acc, List.empty)
              } else {
                val cq =
                  ConjunctiveQuery.parse(id.toInt, query.mkString(" "), prefixes)
                (cq :: acc, List.empty)
              }
            }
            case comment() => (acc, query)
            case _         => (acc, line :: query)
          }
        }
      }
      ._1
      .collect { case Some(q) => q }
    Logger print s"Loaded ${queries.length} queries from $path"
    queries
  }

  /** Load SPARQL queries from files.
    *
    * @param paths list of files containing a conjunctive queries.
    * @param prefixes additional prefixes for the query. It defaults to
    * an empty set.
    *
    * @return a list of [[tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery SelectQuery]] queries.
    *
    * @see [[RDFoxUtil.loadQueriesFromFile()]] for more details on the file format.
    */
  def loadQueriesFromFiles(
      paths: List[os.Path],
      prefixes: Prefixes = new Prefixes()
  ): List[ConjunctiveQuery] =
    paths.flatMap(loadQueriesFromFile(_, prefixes))

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
    val parser = new SPARQLParser(prefixes, new StringReader(query))
    try {
      parser.parseSingleQuery() match {
        case q: SelectQuery => Some(q)
        case _              => None
      }
    } catch {
      /* Just ignore unparsable queries */
      case error: Throwable => {
        Logger print s"Unable to parse '$query' due to error '$error'"
        None
      }
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
      prefixes: Prefixes = RSA.Prefixes,
      opts: RDFoxOpts = RDFoxOpts()
  ): Option[QueryAnswers] =
    parseSelectQuery(query, prefixes).map(submitSelectQuery(data, _, opts))

  /** Returns a query describing an internal predicate.
    *
    * In the RSA combined approach internal predicates are reified to be
    * compatible with RDFox engine. This helper allows to build a query
    * to gather all instances of an internal predicate
    *
    * @param graph named graph to query for the provided predicate
    * @param pred name of the predicate to describe.
    * @param arity arity of the predicate.
    * @return a string containing a SPARQL query.
    */
  def buildDescriptionQuery(
      graph: IRI,
      pred: IRI,
      arity: Int
  ): String = {
    if (arity > 0) {
      val variables = (0 until arity).mkString("?X", " ?X", "")
      s"""
      SELECT $variables
      WHERE {
        GRAPH $graph { ?K a $pred }.
        TT ${TupleTableName.SKOLEM} { $variables ?K } .
      }
      """
    } else {
      s"ASK { GRAPH $graph { ?X a $pred } }"
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
