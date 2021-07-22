package uk.ac.ox.cs.rsacomb.approximation

import java.io.File
import org.semanticweb.owlapi.model.OWLLogicalAxiom

/** Ontology approximation technique. */
trait Approximation {

  /** Approximate an ontology.
    *
    * @param ontology input ontology as a list of axioms
    * @return the approximated ontology
    */
  def approximate(
      ontology: List[OWLLogicalAxiom],
      datafiles: List[File]
  ): List[OWLLogicalAxiom]

}
