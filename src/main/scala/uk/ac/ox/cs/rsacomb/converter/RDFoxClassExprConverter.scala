package uk.ac.ox.cs.rsacomb.converter

import scala.collection.JavaConverters._
import java.util.stream.{Stream, Collectors}

import org.semanticweb.owlapi.model.{
  OWLClassExpression,
  OWLClass,
  OWLObjectSomeValuesFrom,
  OWLDataSomeValuesFrom,
  OWLObjectIntersectionOf,
  OWLObjectOneOf,
  OWLObjectMaxCardinality
}
import org.semanticweb.owlapi.model.OWLClassExpressionVisitorEx
import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  BindAtom,
  TupleTableName,
  TupleTableAtom
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Term,
  Literal,
  Variable,
  FunctionCall,
  IRI
}

import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.OWLObjectProperty

import uk.ac.ox.cs.rsacomb.RSAOntology
import uk.ac.ox.cs.rsacomb.suffix.{RSASuffix, Empty}
import uk.ac.ox.cs.rsacomb.util.RSA

object RDFoxClassExprConverter {

  def apply(
      term: Term,
      unsafe: List[OWLObjectPropertyExpression] = List(),
      skolem: SkolemStrategy = NoSkolem,
      suffix: RSASuffix = Empty
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
    skolem: SkolemStrategy = NoSkolem,
    suffix: RSASuffix = Empty
) extends OWLClassExpressionVisitorEx[RDFoxRuleShards] {

  import uk.ac.ox.cs.rsacomb.implicits.RDFox._

  // OWLClass
  override def visit(expr: OWLClass): RDFoxRuleShards = {
    val iri: IRI = if (expr.isTopEntity()) IRI.THING else expr.getIRI()
    val atom = List(TupleTableAtom.rdf(term, IRI.RDF_TYPE, iri))
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
    // TODO: review nominal handling. Here we are taking "just" one
    val ind = expr.individuals
      .collect(Collectors.toList())
      .asScala
      .filter(_.isOWLNamedIndividual)
      .head // restricts to proper "nominals"
      .asOWLNamedIndividual
      .getIRI
    val atom = List(
      TupleTableAtom.rdf(term, IRI.SAME_AS, ind)
    )
    RDFoxRuleShards(atom, List())
  }

  // OWLObjectSomeValuesFrom
  override def visit(expr: OWLObjectSomeValuesFrom): RDFoxRuleShards = {
    val y = RSAOntology.genFreshVariable()
    // Here we are assuming a role name
    val prop = expr.getProperty()
    // Computes the result of rule skolemization. Depending on the used
    // technique it might involve the introduction of additional atoms,
    // and/or fresh constants and variables.
    val (head, body, term1) = skolem match {
      case NoSkolem    => (List(), List(), y)
      case c: Constant => (List(), List(), c.iri)
      case s: Standard => {
        (
          List(),
          List(
            BindAtom.create(FunctionCall.create("SKOLEM", s.literal, term), y)
          ),
          y
        )
      }
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

  /** Converts a [[org.semanticweb.owlapi.model.OWLDataSomeValuesFrom OWLDataSomeValuesFrom]]
    *
    * @note we assume the expression is "simple", meaning that the
    * property involved is a role name or the inverse of a role name.
    * This assumption will be lifted when we will deal with the
    * normalization of the input ontology.
    *
    * @todo the "filler" of this OWL expression is currently ignored. We
    * need to find a way (if any) to handle
    * [[org.semanticweb.owlapi.model.OWLDataRange OWLDataRange]]
    * in RDFox.
    */
  override def visit(expr: OWLDataSomeValuesFrom): RDFoxRuleShards = {
    val y = RSAOntology.genFreshVariable()
    val prop = expr.getProperty()
    // Computes the result of rule skolemization. Depending on the used
    // technique it might involve the introduction of additional atoms,
    // and/or fresh constants and variables.
    val (head, body, term1) = skolem match {
      case NoSkolem    => (List(), List(), y)
      case c: Constant => (List(), List(), c.iri)
      case s: Standard => {
        (
          List(),
          List(
            BindAtom.create(FunctionCall.create("SKOLEM", s.literal, term), y)
          ),
          y
        )
      }
    }
    val propertyVisitor = new RDFoxPropertyExprConverter(term, term1, suffix)
    val propertyResult = expr.getProperty.accept(propertyVisitor)
    RDFoxRuleShards(head ::: propertyResult, body)
  }

  // OWLObjectMaxCardinality
  override def visit(expr: OWLObjectMaxCardinality): RDFoxRuleShards = {
    val vars =
      List(RSAOntology.genFreshVariable(), RSAOntology.genFreshVariable())
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
      List(TupleTableAtom.rdf(vars(0), IRI.SAME_AS, vars(1))),
      classResult.res ++ propertyResult
    )
  }

  def doDefault(expr: OWLClassExpression): RDFoxRuleShards =
    RDFoxRuleShards(List(), List())

} // class RDFoxClassExprConverter
