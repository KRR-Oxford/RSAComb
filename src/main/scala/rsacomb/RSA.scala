package rsacomb

/* Java imports */
import java.io.File
import java.util.Map

import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.expression.{Variable, IRI}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology
import rsacomb.RSAOntology

// Debug only
import scala.collection.JavaConverters._

object RSA extends RSAOntology {

  val Prefixes = new Prefixes()
  Prefixes.declarePrefix(":", "http://example.com/rsa_example.owl#")
  Prefixes.declarePrefix("internal:", "http://127.0.0.1/")
  Prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  Prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  Prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  // Counter used to implement a simple fresh variable generator
  private var counter = -1;

  def getFreshVariable(): Variable = {
    counter += 1
    Variable.create(f"I$counter%03d")
  }

  def base(name: Any): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get(":").getIRI
        + name.toString
    )

  def internal(name: Any): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get("internal:").getIRI
        + name.toString
    )

  // TODO: move this somewhere else... maybe an OntoUtils class or something.
  def loadOntology(onto: File): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntologyFromOntologyDocument(onto)
  }

} // object RSA
