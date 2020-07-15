package rsacomb

import org.scalatest.{FlatSpec, Matchers, LoneElement}

import uk.ac.manchester.cs.owl.owlapi.{OWLClassImpl, OWLObjectSomeValuesFromImpl, OWLObjectIntersectionOfImpl, OWLObjectOneOfImpl, OWLObjectAllValuesFromImpl, OWLObjectMaxCardinalityImpl, OWLNamedIndividualImpl}
import uk.ac.manchester.cs.owl.owlapi.{OWLObjectPropertyImpl}
import org.semanticweb.owlapi.model.IRI

import tech.oxfordsemantic.jrdfox.logic.{Bind,BuiltinFunctionCall}
import tech.oxfordsemantic.jrdfox.logic.{Atom, Predicate, Term, Variable, Individual}

import rsacomb.RDFoxRuleShards

object OWLClassSpec {

  // IRI
  val iri_Professor = IRI.create("univ:Professor")
  val iri_Female    = IRI.create("std:Female")
  val iri_Student   = IRI.create("univ:Student")
  val iri_Worker    = IRI.create("univ:Worker")
  val iri_alice     = IRI.create("univ:alice")
  val iri_supervises    = IRI.create("univ:supervises")
  val iri_hasSupervisor = IRI.create("univ:hasSupervisor")

  // RDFox Terms
  val term_x = Variable.create("x")
  val term_y = Variable.create("y")
  val term_c1 = Individual.create("internal:c_1")
  val term_c2 = Individual.create("internal:c_2")
  val term_alice = Individual.create("univ:alice")

  // RDFox Predicates
  val pred_sameAs = Predicate.create("owl:sameAs")
  val pred_Professor = Predicate.create(iri_Professor.getIRIString)
  val pred_hasSupervisor = Predicate.create(iri_hasSupervisor.getIRIString)

  // OWL Classes
  // Name Class corresponding to
  //
  //    Professor
  //
  val class_Professor = new OWLClassImpl(iri_Professor)
  val class_Female    = new OWLClassImpl(iri_Female)
  val class_Student   = new OWLClassImpl(iri_Student)
  val class_Worker    = new OWLClassImpl(iri_Worker)
  val class_OWLClass = class_Professor

  // Class Conjunction corresponding to
  //
  //    Female ∧ Student ∧ Worker
  //
  val class_OWLObjectIntersectionOf = 
    new OWLObjectIntersectionOfImpl(
      class_Female, class_Student, class_Worker
    )
  // Singleton Class corresponding to
  //
  //    { alice }
  //
  val class_OWLObjectOneOf =
    new OWLObjectOneOfImpl(
      new OWLNamedIndividualImpl(iri_alice)
    )
  // Object Existential Restiction corresponding to
  //
  //   ∃ hasSupervisor.Professor
  //
  val class_OWLObjectSomeValuesFrom =
    new OWLObjectSomeValuesFromImpl(
      new OWLObjectPropertyImpl(iri_hasSupervisor),
      class_Professor
    )
  // Object Max Cardinality Restriction corresponding to
  //
  //    ≤1 hasSupervisor.Professor
  val class_OWLObjectMaxCardinality =
    new OWLObjectMaxCardinalityImpl(
      new OWLObjectPropertyImpl(iri_hasSupervisor),
      1,
      class_Professor
    )
} // object OWLClassSpec

class OWLClassSpec
  extends FlatSpec with Matchers with LoneElement
{
  // Import required data
  import OWLClassSpec._

  // OWLClass
  class_OWLClass.toString should "be converted into a RDFoxRuleShards" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLClass.accept(visitor)
    result shouldBe a [RDFoxRuleShards]
  }

  it should "have a single Atom in its result list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLClass.accept(visitor)
    result.res.loneElement shouldBe an [Atom]
  }

  it should "have an empty extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLClass.accept(visitor)
    result.ext shouldBe empty
  }

  // OWLObjectIntersectionOf
  class_OWLObjectIntersectionOf.toString should "be converted into a RDFoxRuleShards" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectIntersectionOf.accept(visitor)
    result shouldBe a [RDFoxRuleShards]
  }

  it should "be converted in the union of its converted conjuncts" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result1= class_OWLObjectIntersectionOf.accept(visitor)
    val result2 = RDFoxClassExprConverter.merge(List(
      class_Female.accept(visitor),
      class_Student.accept(visitor),
      class_Worker.accept(visitor)
    ))
    result1.res should contain theSameElementsAs result2.res
    result1.ext should contain theSameElementsAs result2.ext
  }

  // OWLObjectOneOf
  class_OWLObjectOneOf.toString should "be converted into a RDFoxRuleShards" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectOneOf.accept(visitor)
    result shouldBe a [RDFoxRuleShards]
  }

  it should "be converted into a single <owl:sameAs> Atom" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectOneOf.accept(visitor)
    result.res.loneElement should (be (a [Atom]) and have ('predicate (pred_sameAs)))
  }

  it should "have an empty extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectOneOf.accept(visitor)
    result.ext shouldBe empty
  }

  // OWLObjectSomeValuesFrom
  (class_OWLObjectSomeValuesFrom.toString ++ " w/o skolemization") should
    "be converted into a RDFoxRuleShards" in {
      val visitor = RDFoxClassExprConverter(term_x,SkolemStrategy.None)
      val result = class_OWLObjectSomeValuesFrom.accept(visitor)
      result shouldBe a [RDFoxRuleShards]
  }

  it should "have exactly one unary Atom in its result list" in {
    val visitor = RDFoxClassExprConverter(term_x,SkolemStrategy.None)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly (1, result.res) should (be (an [Atom]) and have ('numberOfArguments (1)))
  }

  it should "have exactly one binary Atom in its result list" in {
    val visitor = RDFoxClassExprConverter(term_x,SkolemStrategy.None)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly (1, result.res) should (be (an [Atom]) and have ('numberOfArguments (2)))
  }

  it should "have an empty extension list" in {
    val visitor = RDFoxClassExprConverter(term_x,SkolemStrategy.None)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    result.ext shouldBe empty
  }

  (class_OWLObjectSomeValuesFrom.toString ++ " w/ skolemization") should
    "be converted into a RDFoxRuleShards" in {
      val skolem = SkolemStrategy.Standard(class_OWLObjectSomeValuesFrom.toString)
      val visitor = RDFoxClassExprConverter(term_x,skolem)
      val result = class_OWLObjectSomeValuesFrom.accept(visitor)
      result shouldBe a [RDFoxRuleShards]
  }

  it should "have exactly one unary Atom in its result list" in {
    val skolem = SkolemStrategy.Standard(class_OWLObjectSomeValuesFrom.toString)
    val visitor = RDFoxClassExprConverter(term_x,skolem)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly (1, result.res) should (be (an [Atom]) and have ('numberOfArguments (1)))
  }

  it should "have exactly one binary Atom in its result list" in {
    val skolem = SkolemStrategy.Standard(class_OWLObjectSomeValuesFrom.toString)
    val visitor = RDFoxClassExprConverter(term_x,skolem)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly (1, result.res) should (be (an [Atom]) and have ('numberOfArguments (2)))
  }

  it should "should have a single SKOLEM call in the extension list" in {
    val skolem = SkolemStrategy.Standard(class_OWLObjectSomeValuesFrom.toString)
    val visitor = RDFoxClassExprConverter(term_x,skolem)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    result.ext.loneElement shouldBe a [Bind]
    val builtin = result.ext.head.asInstanceOf[Bind].getBuiltinExpression
    builtin should (be (a [BuiltinFunctionCall]) and have ('functionName ("SKOLEM")))
  }

  (class_OWLObjectSomeValuesFrom.toString ++ " w/ constant skolemization") should
    "be converted into a RDFoxRuleShards" in {
      val skolem = SkolemStrategy.Constant(class_OWLObjectSomeValuesFrom.toString)
      val visitor = RDFoxClassExprConverter(term_x,skolem)
      val result = class_OWLObjectSomeValuesFrom.accept(visitor)
      result shouldBe a [RDFoxRuleShards]
  }

  it should "have exactly one unary Atom in its result list" in {
    val skolem = SkolemStrategy.Constant(class_OWLObjectSomeValuesFrom.toString)
    val visitor = RDFoxClassExprConverter(term_x,skolem)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly (1, result.res) should (be (an [Atom]) and have ('numberOfArguments (1)))
  }

  it should "have exactly one binary Atom in its result list" in {
    val skolem = SkolemStrategy.Constant(class_OWLObjectSomeValuesFrom.toString)
    val visitor = RDFoxClassExprConverter(term_x,skolem)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly (1, result.res) should (be (an [Atom]) and have ('numberOfArguments (2)))
  }

  it should "have an empty extension list" in {
    val skolem = SkolemStrategy.Constant(class_OWLObjectSomeValuesFrom.toString)
    val visitor = RDFoxClassExprConverter(term_x,skolem)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    result.ext shouldBe empty
  }

  // OWLObjectMaxCardinalityImpl
  class_OWLObjectMaxCardinality.toString should
    "be converted into a RDFoxRuleShards" in {
      val visitor = RDFoxClassExprConverter(term_x)
      val result = class_OWLObjectMaxCardinality.accept(visitor)
      result shouldBe a [RDFoxRuleShards]
  }

  it should "have a single <owl:sameAs> Atom in the result list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectMaxCardinality.accept(visitor)
    result.res.loneElement should (be (an [Atom]) and have ('predicate (pred_sameAs)))
  }

  it should "have two unary Atoms in its extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectMaxCardinality.accept(visitor)
    exactly (2, result.ext) should (be (an [Atom]) and have ('numberOfArguments (1)))
  }

  it should "have two binary Atoms in its extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectMaxCardinality.accept(visitor)
    exactly (2, result.ext) should (be (an [Atom]) and have ('numberOfArguments (2)))
  }

} // class OWLClassSpec
