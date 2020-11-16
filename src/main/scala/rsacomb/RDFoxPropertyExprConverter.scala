package rsacomb

import org.semanticweb.owlapi.model.{OWLPropertyExpression, OWLObjectProperty}
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx

import tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, IRI, Literal}

import org.semanticweb.owlapi.model.OWLObjectInverseOf

import suffix.{RSASuffix, Inverse}

class RDFoxPropertyExprConverter(
    term1: Term,
    term2: Term,
    suffix: RSASuffix
) extends OWLPropertyExpressionVisitorEx[List[TupleTableAtom]] {

  // Automatically converts OWLAPI types into RDFox equivalent types.
  import RDFoxUtil.owlapi2rdfox;

  override def visit(expr: OWLObjectProperty): List[TupleTableAtom] = {
    //val pred = IRI.create(expr.getIRI.getIRIString ++ suffix.getSuffix)
    val pred = IRI.create(expr :: suffix)
    List(TupleTableAtom.rdf(term1, pred, term2))
  }

  override def visit(expr: OWLObjectInverseOf): List[TupleTableAtom] = {
    //expr.getInverse.getNamedProperty.getIRI.getIRIString ++ suffix.getSuffix ++ "_inv"
    val pred = IRI.create(expr :: suffix + Inverse)
    List(TupleTableAtom.rdf(term1, pred, term2))
  }

  def doDefault(expr: OWLPropertyExpression): List[TupleTableAtom] = List()

} // class RDFoxPropertyExprConverter
