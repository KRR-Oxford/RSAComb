package uk.ac.ox.cs.rsacomb.approximation

import java.io.File
import org.semanticweb.owlapi.model.OWLLogicalAxiom

/** Ontology approximation technique. */
trait Approximation {

  /** Approximate an ontology.
    *
    * @param ontology input ontology
    * @return a new approximated ontology
    */
  def approximate(
      ontology: List[OWLLogicalAxiom],
      datafiles: List[File]
  ): List[OWLLogicalAxiom]

}
