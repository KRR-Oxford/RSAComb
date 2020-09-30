package rsacomb

import org.semanticweb.owlapi.model.{OWLPropertyExpression, OWLObjectProperty}
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx

import tech.oxfordsemantic.jrdfox.logic.{Atom, Term, IRI, Literal}

import org.semanticweb.owlapi.model.OWLObjectInverseOf

class RDFoxPropertyExprConverter(
    term1: Term,
    term2: Term,
    suffix: RSASuffix
) extends OWLPropertyExpressionVisitorEx[List[Atom]] {

  // Automatically converts OWLAPI types into RDFox equivalent types.
  import RDFoxUtil.owlapi2rdfox;

  override def visit(expr: OWLObjectProperty): List[Atom] = {
    val pred = IRI.create(expr.getIRI.getIRIString ++ suffix.getSuffix)
    List(Atom.rdf(term1, pred, term2))
  }

  override def visit(expr: OWLObjectInverseOf): List[Atom] = {
    val pred = IRI.create(
      expr.getInverse.getNamedProperty.getIRI.getIRIString ++ suffix.getSuffix ++ "_inv"
    )
    List(Atom.rdf(term1, pred, term2))
  }

  def doDefault(expr: OWLPropertyExpression): List[Atom] = List()

} // class RDFoxPropertyExprConverter
