package rsacomb

/* Java imports */
import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology

object RSA extends RSAOntology {

  val PrefixBase = "http://example.com/rsa_example.owl#"
  val PrefixInternal = "http://127.0.0.1/"
  val PredicatePE = PrefixInternal + "PE"
  val PredicateU = PrefixInternal + "U"
  val PredicateE = PrefixInternal + "E"

  // TODO: move this somewhere else... maybe an OntoUtils class or something.
  def loadOntology(onto: File): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntologyFromOntologyDocument(onto)
  }

} // object RSA
