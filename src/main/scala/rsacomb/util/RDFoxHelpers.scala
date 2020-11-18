package rsacomb.util

import java.util.{Map => JMap, HashMap => JHashMap}
import java.io.StringReader
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.client.{
  ConnectionFactory,
  ServerConnection,
  DataStoreConnection
}
import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.logic.expression.Resource
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

import rsacomb.suffix.Nth

object RDFoxHelpers {

  def openConnection(
      dataStore: String,
      opts: JMap[String, String] = new JHashMap[String, String]()
  ): (ServerConnection, DataStoreConnection) = {
    /* Create local server connection
     */
    val serverUrl = "rdfox:local"
    val role = ""
    val password = ""
    val server =
      ConnectionFactory.newServerConnection(serverUrl, role, password)

    /* Create datastore connection
     */
    // parameters.put("owl-in-rdf-support", "relaxed")
    // parameters.put("equality", "noUNA")
    server.createDataStore(dataStore, "par-complex-nn", opts)
    val data = server.newDataStoreConnection(dataStore)

    (server, data)
  }

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

  def submitSelectQuery(
      data: DataStoreConnection,
      query: String,
      prefixes: Prefixes = new Prefixes(),
      opts: JMap[String, String] = new JHashMap[String, String]()
  ): List[List[Resource]] = {
    val cursor = data.createCursor(prefixes, query, opts)
    var answers: List[List[Resource]] = List()
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

  def queryInternalPredicate(
      data: DataStoreConnection,
      pred: String,
      arity: Int,
      opts: JMap[String, String] = new JHashMap[String, String]()
  ): List[List[Resource]] = {
    var query = "SELECT"
    for (i <- 0 until arity) {
      query ++= s" ?X$i"
    }
    query ++= " WHERE {"
    for (i <- 0 until arity) {
      query ++= s" ?S rsa:${pred :: Nth(i)} ?X$i ."
    }
    query ++= " }"
    submitSelectQuery(data, query, RSA.Prefixes, opts)
  }

  def closeConnection(
      server: ServerConnection,
      data: DataStoreConnection
  ): Unit = {
    server.close();
    data.close();
  }

}
