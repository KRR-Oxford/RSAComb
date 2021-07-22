package uk.ac.ox.cs.rsacomb.approximation

import java.io.File
import org.semanticweb.owlapi.model.OWLLogicalAxiom

import uk.ac.ox.cs.rsacomb.ontology.Ontology

/** Ontology approximation technique. */
trait Approximation[T] {

  /** Approximate an ontology.
    *
    * @param ontology input ontology as a list of axioms
    * @return the approximated ontology
    */
  def approximate(ontology: Ontology): T

}
