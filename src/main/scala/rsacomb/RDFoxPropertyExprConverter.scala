package rsacomb

import org.semanticweb.owlapi.model.{OWLPropertyExpression, OWLObjectProperty}
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx

import tech.oxfordsemantic.jrdfox.logic.{Atom, Term, IRI, Literal}

import rsacomb.SkolemStrategy
import org.semanticweb.owlapi.model.OWLObjectInverseOf

class RDFoxPropertyExprConverter(
    term1: Term,
    term2: Term,
    skolem: SkolemStrategy
) extends OWLPropertyExpressionVisitorEx[List[Atom]] {

  // Automatically converts OWLAPI types into RDFox equivalent types.
  import RDFoxUtil.owlapi2rdfox;

  override def visit(expr: OWLObjectProperty): List[Atom] =
    List(Atom.rdf(term1, expr.getIRI, term2))

  override def visit(expr: OWLObjectInverseOf): List[Atom] = {
    val name = expr.getInverse.getNamedProperty.getIRI.getIRIString;
    List(Atom.rdf(term1, IRI.create(name ++ "_inv"), term2))
  }

  def doDefault(expr: OWLPropertyExpression): List[Atom] = List()

} // class RDFoxPropertyExprConverter
