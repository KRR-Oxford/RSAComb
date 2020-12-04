package uk.ac.ox.cs.rsacomb
package object converter {

  import org.semanticweb.owlapi.model.OWLAxiom

  implicit def axiomToHashedString(axiom: OWLAxiom): String =
    s"${axiom.toString.hashCode}"

}
