package uk.ac.ox.cs.rsacomb.converter

import org.semanticweb.owlapi.model.{OWLPropertyExpression, OWLObjectProperty}
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx

import tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, IRI, Literal}

import org.semanticweb.owlapi.model.OWLObjectInverseOf

import uk.ac.ox.cs.rsacomb.suffix.{RSASuffix, Inverse}

class RDFoxPropertyExprConverter(
    term1: Term,
    term2: Term,
    suffix: RSASuffix
) extends OWLPropertyExpressionVisitorEx[List[TupleTableAtom]] {

  // Automatically converts OWLAPI types into RDFox equivalent types.
  import uk.ac.ox.cs.rsacomb.implicits.RDFox._

  override def visit(expr: OWLObjectProperty): List[TupleTableAtom] = {
    val base = expr.getIRI.getIRIString
    val pred = IRI.create(base :: suffix)
    List(TupleTableAtom.rdf(term1, pred, term2))
  }

  override def visit(expr: OWLObjectInverseOf): List[TupleTableAtom] = {
    val visitor = new RDFoxPropertyExprConverter(term1, term2, suffix + Inverse)
    expr.getInverse.accept(visitor)
  }

  def doDefault(expr: OWLPropertyExpression): List[TupleTableAtom] = List()

} // class RDFoxPropertyExprConverter
