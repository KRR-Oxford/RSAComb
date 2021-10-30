package uk.ac.ox.cs.rsacomb.util

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLClass
import tech.oxfordsemantic.jrdfox.logic.expression.Variable

/** Simple fresh variable/class generator */
object DataFactory {

  /** Manager instance to interface with OWLAPI */
  private val manager = OWLManager.createOWLOntologyManager()
  private val factory = manager.getOWLDataFactory()

  def apply(counter: Integer = -1): DataFactory = new DataFactory(counter)
}

class DataFactory(private var counter: Integer) {

  private def getNext(): Integer = {
    counter += 1
    counter
  }

  def getVariable(): Variable =
    Variable.create(f"I${this.getNext()}%05d")

  def getOWLClass(): OWLClass =
    DataFactory.factory.getOWLClass(RSA(s"tmp${this.getNext()}").getIRI)
}
