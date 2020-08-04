package rsacomb

import scala.collection.JavaConverters._
import java.util.stream.{Stream,Collectors}

import org.semanticweb.owlapi.model.{OWLClassExpression, OWLClass, OWLObjectSomeValuesFrom, OWLObjectIntersectionOf, OWLObjectOneOf, OWLObjectMaxCardinality}
import org.semanticweb.owlapi.model.OWLClassExpressionVisitorEx
import tech.oxfordsemantic.jrdfox.logic.{BindAtom, BuiltinFunctionCall, TupleTableName}
import tech.oxfordsemantic.jrdfox.logic.{Atom, Term, Variable, Literal, Datatype}

import rsacomb.SkolemStrategy
import rsacomb.RDFoxRuleShards
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLObjectProperty

object RDFoxClassExprConverter {

  def apply(
    term : Term = Variable.create("x"),
    skolem : SkolemStrategy = SkolemStrategy.None,
    unsafe : List[OWLObjectPropertyExpression] = List()
  ) : RDFoxClassExprConverter =
	  new RDFoxClassExprConverter(term, skolem, unsafe)

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

class RDFoxClassExprConverter(term : Term, skolem : SkolemStrategy, unsafe : List[OWLObjectPropertyExpression])
  extends OWLClassExpressionVisitorEx[RDFoxRuleShards]
{

  // OWLClass
  override
  def visit(expr : OWLClass) : RDFoxRuleShards = {
    val name = expr.getIRI.getIRIString
    val atom = List(Atom.create(TupleTableName.create(name), term))
    RDFoxRuleShards(atom,List())
  }

  // OWLObjectIntersectionOf
  override
  def visit(expr : OWLObjectIntersectionOf) : RDFoxRuleShards = {
    val visitor = new RDFoxClassExprConverter(term, skolem, unsafe)
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
      TupleTableName.create("owl:sameAs"), term, Literal.create(ind, Datatype.IRI_REFERENCE)
    ))
    RDFoxRuleShards(atom,List())
  }

  // OWLObjectSomeValuesFrom
  override
  def visit(expr : OWLObjectSomeValuesFrom) : RDFoxRuleShards = {
    // TODO: variables needs to be handled at visitor level. Hardcoding
    // the name of the varibles might lead to errors for complex cases.
    val y = Variable.create("y")
    val prop = expr.getProperty()
    // Computes the result of rule skolemization. Depending on the used 
    // technique it might involve the introduction of additional atoms,
    // and/or fresh constants and variables.
    val (head, body, term1) = skolem match {
        case SkolemStrategy.None => (List(), List(), y)
        case SkolemStrategy.Constant(c) => (List(), List(), Literal.create(c, Datatype.IRI_REFERENCE))
        case SkolemStrategy.ConstantRSA(c) => {
          val lit = Literal.create(c, Datatype.IRI_REFERENCE)
          if (unsafe.contains(prop))
            (List(Atom.create(TupleTableName.create("internal:PE"),term,lit), Atom.create(TupleTableName.create("internal:U"),lit)), List(), lit)
          else 
            (List(), List(), lit)
        }
        case SkolemStrategy.Standard(f) => 
          // At the time of writing the RDFox library does not have a
          // particular class for the "SKOLEM" operator and it is instead
          // a simple builtin function with a "special" name.
          (List(),List(BindAtom.create(BuiltinFunctionCall.create("SKOLEM",term),y)),y)
    }
    val classVisitor = new RDFoxClassExprConverter(term1, skolem, unsafe)
    val classResult = expr.getFiller.accept(classVisitor)
    val propertyVisitor = new RDFoxPropertyExprConverter(term, term1, skolem)
    val propertyResult = expr.getProperty.accept(propertyVisitor)
    RDFoxRuleShards(
      classResult.res ++ propertyResult ++ head,
      classResult.ext ++ body
    )
  }

  // OWLObjectMaxCardinality
  override
  def visit(expr : OWLObjectMaxCardinality) : RDFoxRuleShards = {
    // TODO: again, no hardcoded variables
    val vars = List(Variable.create("y"),Variable.create("z"))
    val classResult = RDFoxClassExprConverter.merge(
      vars.map(new RDFoxClassExprConverter(_,skolem, unsafe))
          .map(expr.getFiller.accept(_))
    )
    val propertyResult = 
      vars.map(new RDFoxPropertyExprConverter(term,_,skolem))
          .map(expr.getProperty.accept(_))
          .flatten
    RDFoxRuleShards(
      List(Atom.create(TupleTableName.create("owl:sameAs"),vars(0),vars(1))),
      classResult.res ++ propertyResult
    )
  }
	
  def doDefault(expr : OWLClassExpression) : RDFoxRuleShards =
    RDFoxRuleShards(List(),List())

} // class RDFoxClassExprConverter
