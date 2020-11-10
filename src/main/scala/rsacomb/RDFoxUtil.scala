package rsacomb

/* Java imports */
import java.util.HashMap
import java.io.StringReader
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.{Query, SelectQuery}
import tech.oxfordsemantic.jrdfox.client.{
  ConnectionFactory,
  ServerConnection,
  DataStoreConnection
}
import tech.oxfordsemantic.jrdfox.formats.SPARQLParser

import tech.oxfordsemantic.jrdfox.logic.expression.{IRI => RDFox_IRI}
import org.semanticweb.owlapi.model.{IRI => OWL_IRI}

object RDFoxUtil {

  implicit def rdfox2owlapi(iri: RDFox_IRI): OWL_IRI = {
    OWL_IRI.create(iri.getIRI)
  }

  implicit def owlapi2rdfox(iri: OWL_IRI): RDFox_IRI = {
    RDFox_IRI.create(iri.getIRIString())
  }

  implicit def owlapi2rdfox(iri: String): RDFox_IRI = {
    RDFox_IRI.create(iri)
  }

  def openConnection(
      dataStore: String
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
    val parameters = new HashMap[String, String]()
    parameters.put("owl-in-rdf-support", "relaxed")
    //parameters.put("equality", "noUNA")
    server.createDataStore(dataStore, "par-complex-nn", parameters)
    val data = server.newDataStoreConnection(dataStore)

    (server, data)
  }

  def parseQuery(
      query: String,
      prefixes: Prefixes = RSA.Prefixes
  ): Option[SelectQuery] = {
    val parser = new SPARQLParser(
      prefixes,
      new StringReader(query)
    )
    // NOTE: return only conjunctive queries for now (SelectQuery)
    parser.parseSingleQuery() match {
      case q: SelectQuery => Some(q)
      case _              => None
    }
  }

  def submitQuery(
      data: DataStoreConnection,
      prefixes: Prefixes,
      query: String,
      answers: Int
  ): Unit = {
    println(s"\nQUERY {\n$query\n}")
    val cursor = data.createCursor(
      prefixes,
      query,
      new HashMap[String, String]()
    );
    var mul = cursor.open()
    while (mul > 0) {
      print("Answer: ")
      for (i <- 0 until answers) {
        val res = cursor.getResource(i)
        print(s"$res ")
      }
      println()
      mul = cursor.advance()
    }
    cursor.close();
    println(s"QUERY END")
  }

  def closeConnection(
      server: ServerConnection,
      data: DataStoreConnection
  ): Unit = {
    server.close();
    data.close();
  }

} // object RDFoxUtil
