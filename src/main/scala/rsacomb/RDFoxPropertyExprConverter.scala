package rsacomb

import org.semanticweb.owlapi.model.{OWLPropertyExpression, OWLObjectProperty}
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx

import tech.oxfordsemantic.jrdfox.logic.{TupleTableName}
import tech.oxfordsemantic.jrdfox.logic.{Atom, Term, Variable, Literal}

import rsacomb.SkolemStrategy

class RDFoxPropertyExprConverter(term1 : Term, term2 : Term, skolem : SkolemStrategy)
  extends OWLPropertyExpressionVisitorEx[List[Atom]]
{

  override
  def visit(expr : OWLObjectProperty) : List[Atom] = {
    val name = expr.getIRI.getIRIString
    List(Atom.create(TupleTableName.create(name), term1, term2))
  }

  def doDefault(expr : OWLPropertyExpression) : List[Atom] = List()

} // class RDFoxPropertyExprConverter

