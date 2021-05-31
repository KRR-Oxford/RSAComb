package uk.ac.ox.cs.rsacomb.approximation

import java.io.File
import org.semanticweb.owlapi.model.OWLAxiom

/** Ontology approximation technique. */
trait Approximation {

  /** Approximate an ontology.
    *
    * @param ontology input ontology
    * @return a new approximated ontology
    */
  def approximate(ontology: Seq[OWLAxiom], datafiles: Seq[File]): Seq[OWLAxiom]

}
