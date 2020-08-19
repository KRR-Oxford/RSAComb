package rsacomb

/* Java imports */
import java.io.File
import java.util.Map

import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.IRI
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology

object RSA extends RSAOntology {

  val Prefixes = new Prefixes()
  Prefixes.declarePrefix(":", "http://example.com/rsa_example.owl#")
  Prefixes.declarePrefix("internal:", "http://127.0.0.1/")
  Prefixes.declarePrefix("rdf:", "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
  Prefixes.declarePrefix("rdfs:", "http://www.w3.org/2000/01/rdf-schema#")
  Prefixes.declarePrefix("owl:", "http://www.w3.org/2002/07/owl#")

  def internal(name: String): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get("internal:").getIRI + name
    )

  // TODO: move this somewhere else... maybe an OntoUtils class or something.
  def loadOntology(onto: File): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntologyFromOntologyDocument(onto)
  }

} // object RSA
