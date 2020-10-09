package rsacomb

import scala.collection.JavaConverters._
import java.util.stream.{Stream, Collectors}

import org.semanticweb.owlapi.model.{
  OWLClassExpression,
  OWLClass,
  OWLObjectSomeValuesFrom,
  OWLObjectIntersectionOf,
  OWLObjectOneOf,
  OWLObjectMaxCardinality
}
import org.semanticweb.owlapi.model.OWLClassExpressionVisitorEx
import tech.oxfordsemantic.jrdfox.logic.{
  BindAtom,
  BuiltinFunctionCall,
  TupleTableName
}
import tech.oxfordsemantic.jrdfox.logic.{
  Atom,
  Term,
  Variable,
  Literal,
  Datatype,
  IRI
}

import rsacomb.SkolemStrategy
import rsacomb.RDFoxRuleShards
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLObjectProperty

object RDFoxClassExprConverter {

  def apply(
      term: Term,
      unsafe: List[OWLObjectPropertyExpression] = List(),
      skolem: SkolemStrategy = SkolemStrategy.None,
      suffix: RSASuffix = RSASuffix.None
  ): RDFoxClassExprConverter =
    new RDFoxClassExprConverter(term, unsafe, skolem, suffix)

  def merge(rules: List[RDFoxRuleShards]): RDFoxRuleShards = {
    rules.foldLeft(RDFoxRuleShards(List(), List())) { (r1, r2) =>
      RDFoxRuleShards(
        r1.res ++ r2.res,
        r1.ext ++ r2.ext
      )
    }
  }

} // object RDFoxClassExprConverter

class RDFoxClassExprConverter(
    term: Term,
    unsafe: List[OWLObjectPropertyExpression],
    skolem: SkolemStrategy,
    suffix: RSASuffix
) extends OWLClassExpressionVisitorEx[RDFoxRuleShards] {

  import RDFoxUtil.owlapi2rdfox;

  // OWLClass
  override def visit(expr: OWLClass): RDFoxRuleShards = {
    val iri: IRI = if (expr.isTopEntity()) IRI.THING else expr.getIRI()
    val atom = List(Atom.rdf(term, IRI.RDF_TYPE, iri))
    RDFoxRuleShards(atom, List())
  }

  // OWLObjectIntersectionOf
  override def visit(expr: OWLObjectIntersectionOf): RDFoxRuleShards = {
    val visitor = new RDFoxClassExprConverter(term, unsafe, skolem, suffix)
    // TODO: maybe using `flatMap` instead of `merge` + `map` works as well
    RDFoxClassExprConverter.merge(
      expr.asConjunctSet.asScala.toList
        .map((e: OWLClassExpression) => e.accept(visitor))
    )
  }

  // OWLObjectOneOf
  override def visit(expr: OWLObjectOneOf): RDFoxRuleShards = {
    val visitor = RDFoxClassExprConverter(term, unsafe, skolem, suffix)
    // TODO: review nominal handling. Here we are taking "just" one
    val ind = expr.individuals
      .collect(Collectors.toList())
      .asScala
      .filter(_.isOWLNamedIndividual)
      .head // restricts to proper "nominals"
      .asOWLNamedIndividual
      .getIRI
    val atom = List(
      Atom.sameAs(term, ind)
    )
    RDFoxRuleShards(atom, List())
  }

  // OWLObjectSomeValuesFrom
  override def visit(expr: OWLObjectSomeValuesFrom): RDFoxRuleShards = {
    val y = RSA.getFreshVariable()
    // Here we are assuming a role name
    val prop = expr.getProperty()
    // Computes the result of rule skolemization. Depending on the used
    // technique it might involve the introduction of additional atoms,
    // and/or fresh constants and variables.
    val (head, body, term1) = skolem match {
      case SkolemStrategy.None        => (List(), List(), y)
      case SkolemStrategy.Constant(c) => (List(), List(), c)
      case SkolemStrategy.ConstantRSA(c) => {
        if (unsafe.contains(prop))
          (
            List(
              Atom.rdf(term, RSA.internal("PE"), c),
              Atom.rdf(c, IRI.RDF_TYPE, RSA.internal("U"))
            ),
            List(),
            c
          )
        else
          (List(), List(), c)
      }
      case SkolemStrategy.Standard(f) =>
        // particular class for the "SKOLEM" operator and it is instead
        // a simple builtin function with a "special" name.
        (
          List(),
          List(BindAtom.create(BuiltinFunctionCall.create("SKOLEM", term), y)),
          y
        )
    }
    val classVisitor =
      new RDFoxClassExprConverter(term1, unsafe, skolem, suffix)
    val classResult = expr.getFiller.accept(classVisitor)
    val propertyVisitor = new RDFoxPropertyExprConverter(term, term1, suffix)
    val propertyResult = expr.getProperty.accept(propertyVisitor)
    RDFoxRuleShards(
      classResult.res ++ propertyResult ++ head,
      classResult.ext ++ body
    )
  }

  // OWLObjectMaxCardinality
  override def visit(expr: OWLObjectMaxCardinality): RDFoxRuleShards = {
    // TODO: again, no hardcoded variables
    val vars = List(RSA.getFreshVariable(), RSA.getFreshVariable())
    val classResult = RDFoxClassExprConverter.merge(
      vars
        .map(new RDFoxClassExprConverter(_, unsafe, skolem, suffix))
        .map(expr.getFiller.accept(_))
    )
    val propertyResult =
      vars
        .map(new RDFoxPropertyExprConverter(term, _, suffix))
        .map(expr.getProperty.accept(_))
        .flatten
    RDFoxRuleShards(
      List(Atom.sameAs(vars(0), vars(1))),
      classResult.res ++ propertyResult
    )
  }

  def doDefault(expr: OWLClassExpression): RDFoxRuleShards =
    RDFoxRuleShards(List(), List())

} // class RDFoxClassExprConverter
