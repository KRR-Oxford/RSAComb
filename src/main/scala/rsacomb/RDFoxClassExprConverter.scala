package rsacomb

import scala.collection.JavaConverters._
import java.util.stream.{Stream,Collectors}

import org.semanticweb.owlapi.model.{OWLClassExpression, OWLClass, OWLObjectSomeValuesFrom, OWLObjectIntersectionOf, OWLObjectOneOf, OWLObjectMaxCardinality}
import org.semanticweb.owlapi.model.OWLClassExpressionVisitorEx
import tech.oxfordsemantic.jrdfox.logic.{AtomicFormula, Bind,BuiltinFunctionCall}
import tech.oxfordsemantic.jrdfox.logic.{Atom, Predicate, Term, Variable, Literal, Individual}

import rsacomb.SkolemStrategy
import rsacomb.RDFoxRuleShards

object RDFoxClassExprConverter {

  def apply(term : Term, skolem : SkolemStrategy) : RDFoxClassExprConverter =
	new RDFoxClassExprConverter(term, skolem)

  def apply(term : Term) : RDFoxClassExprConverter =
	new RDFoxClassExprConverter(term, SkolemStrategy.None)

  def merge(rules : List[RDFoxRuleShards]) : RDFoxRuleShards = {
    rules.foldLeft(RDFoxRuleShards(List(),List())) {
      (r1,r2) =>
        RDFoxRuleShards(
          r1.res ++ r2.res,
          r1.ext ++ r2.ext
        )
    }
  }

} // object RDFoxClassExprConverter

class RDFoxClassExprConverter(term : Term, skolem : SkolemStrategy)
  extends OWLClassExpressionVisitorEx[RDFoxRuleShards]
{

  // OWLClass
  override
  def visit(expr : OWLClass) : RDFoxRuleShards = {
    val name = expr.getIRI.getIRIString
    val atom = List(Atom.create(Predicate.create(name), term))
    RDFoxRuleShards(atom,List())
  }

  // OWLObjectIntersectionOf
  override
  def visit(expr : OWLObjectIntersectionOf) : RDFoxRuleShards = {
    val visitor = new RDFoxClassExprConverter(term,skolem)
    // TODO: maybe using `flatMap` instead of `merge` + `map` works as well
	RDFoxClassExprConverter.merge (
      expr.asConjunctSet.asScala.toList
          .map((e : OWLClassExpression) => e.accept(visitor))
    )
  }

  // OWLObjectOneOf
  override
  def visit(expr : OWLObjectOneOf) : RDFoxRuleShards = {
    val visitor = RDFoxClassExprConverter(term,skolem)
    // TODO: review nominal handling. Here we are taking "just" one
    val ind = expr.individuals.collect(Collectors.toList()).asScala
                  .filter(_.isOWLNamedIndividual)
                  .head // restricts to proper "nominals"
                  .asOWLNamedIndividual.getIRI.getIRIString
    val atom = List(Atom.create(
      Predicate.create("owl:sameAs"), term, Individual.create(ind)
    ))
    RDFoxRuleShards(atom,List())
  }

  // OWLObjectSomeValuesFrom
  override
  def visit(expr : OWLObjectSomeValuesFrom) : RDFoxRuleShards = {
    // TODO: variables needs to be handled at visitor level. Hardcoding
    // the name of the varibles might lead to errors for complex cases.
    val y = Variable.create("y")
	val (fun,term1) = skolem match {
      case SkolemStrategy.None => (List(),y)
      case SkolemStrategy.Constant(c) => (List(), Individual.create(c))
      case SkolemStrategy.Standard(f) => 
        // At the time of writing the RDFox library does not have a
        // particular class for the "SKOLEM" operator and it is instead
        // a simple builtin function with a special name.
        (List(Bind.create(BuiltinFunctionCall.create("SKOLEM",term),y)),y)
	}
    val classVisitor = new RDFoxClassExprConverter(term1,skolem)
    val classResult = expr.getFiller.accept(classVisitor)
    val propertyVisitor = new RDFoxPropertyExprConverter(term, term1, skolem)
    val propertyResult = expr.getProperty.accept(propertyVisitor)
    RDFoxRuleShards(
      classResult.res ++ propertyResult,
      fun ++ classResult.ext
    )
  }

  // OWLObjectMaxCardinality
  override
  def visit(expr : OWLObjectMaxCardinality) : RDFoxRuleShards = {
    // TODO: again, no hardcoded variables
    val vars = List(Variable.create("y"),Variable.create("z"))
    val classResult = RDFoxClassExprConverter.merge(
      vars.map(new RDFoxClassExprConverter(_,skolem))
          .map(expr.getFiller.accept(_))
    )
    val propertyResult = 
      vars.map(new RDFoxPropertyExprConverter(term,_,skolem))
          .map(expr.getProperty.accept(_))
          .flatten
    RDFoxRuleShards(
      List(Atom.create(Predicate.create("owl:sameAs"),vars(0),vars(1))),
      classResult.res ++ propertyResult
    )
  }
	
  def doDefault(expr : OWLClassExpression) : RDFoxRuleShards =
    RDFoxRuleShards(List(),List())

} // class RDFoxClassExprConverter
