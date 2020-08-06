package rsacomb

/* Java imports */
import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntology

object RSA extends RSAOntology {

  // TODO: move this somewhere else... maybe an OntoUtils class or something.
  def loadOntology(onto: File ): OWLOntology = {
    val manager = OWLManager.createOWLOntologyManager()
    manager.loadOntologyFromOntologyDocument(onto)
  }

} // object RSA
