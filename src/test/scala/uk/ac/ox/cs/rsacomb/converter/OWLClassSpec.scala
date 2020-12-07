package uk.ox.ac.cs.rsacomb.converter

import java.util.{ArrayList => JList}

import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.semanticweb.owlapi.model.OWLClassExpression
import uk.ac.manchester.cs.owl.owlapi.{
  OWLClassImpl,
  OWLObjectSomeValuesFromImpl,
  OWLObjectIntersectionOfImpl,
  OWLObjectOneOfImpl,
  OWLObjectAllValuesFromImpl,
  OWLObjectMaxCardinalityImpl,
  OWLNamedIndividualImpl
}
import uk.ac.manchester.cs.owl.owlapi.{OWLObjectPropertyImpl}
import org.semanticweb.owlapi.model.IRI
import tech.oxfordsemantic.jrdfox.logic.expression.{IRI => RDFIRI}

import tech.oxfordsemantic.jrdfox.logic.Datatype
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  TupleTableAtom,
  TupleTableName,
  BindAtom
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  FunctionCall,
  Term,
  Variable,
  Literal
}

import uk.ac.ox.cs.rsacomb.converter.{
  RDFoxRuleShards,
  RDFoxClassExprConverter,
  SkolemStrategy,
  Standard,
  Constant
}
import uk.ac.ox.cs.rsacomb.util.RSA

object OWLClassSpec {

  // IRI
  val iri_Professor = IRI.create("univ:Professor")
  val iri_Female = IRI.create("std:Female")
  val iri_Student = IRI.create("univ:Student")
  val iri_Worker = IRI.create("univ:Worker")
  val iri_alice = IRI.create("univ:alice")
  val iri_supervises = IRI.create("univ:supervises")
  val iri_hasSupervisor = IRI.create("univ:hasSupervisor")

  // RDFox Terms
  val term_x = Variable.create("x")
  val term_y = Variable.create("y")
  val term_c1 = RSA("c_1")
  val term_c2 = RSA("c_2")
  val term_alice = RDFIRI.create("univ:alice")

  // RDFox Predicates
  val pred_sameAs = TupleTableName.create("owl:sameAs")
  val pred_Professor = TupleTableName.create(iri_Professor.getIRIString)
  val pred_hasSupervisor = TupleTableName.create(iri_hasSupervisor.getIRIString)

  // OWL Classes
  // Name Class corresponding to
  //
  //    Professor
  //
  val class_Professor = new OWLClassImpl(iri_Professor)
  val class_Female = new OWLClassImpl(iri_Female)
  val class_Student = new OWLClassImpl(iri_Student)
  val class_Worker = new OWLClassImpl(iri_Worker)
  val class_OWLClass = class_Professor

  // Class Conjunction corresponding to
  //
  //    Female ∧ Student ∧ Worker
  //
  val class_OWLObjectIntersectionOf = {
    val conjuncts = new JList[OWLClassExpression]()
    conjuncts.add(class_Female)
    conjuncts.add(class_Student)
    conjuncts.add(class_Worker)
    new OWLObjectIntersectionOfImpl(conjuncts)
  }
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

class OWLClassSpec extends AnyFlatSpec with Matchers with LoneElement {
  // Import required data
  import OWLClassSpec._

  // OWLClass
  class_OWLClass.toString should "be converted into a RDFoxRuleShards" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLClass.accept(visitor)
    result shouldBe a[RDFoxRuleShards]
  }

  it should "have a single TupleTableAtom in its result list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLClass.accept(visitor)
    result.res.loneElement shouldBe an[TupleTableAtom]
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
    result shouldBe a[RDFoxRuleShards]
  }

  it should "be converted in the union of its converted conjuncts" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result1 = class_OWLObjectIntersectionOf.accept(visitor)
    val result2 = RDFoxClassExprConverter.merge(
      List(
        class_Female.accept(visitor),
        class_Student.accept(visitor),
        class_Worker.accept(visitor)
      )
    )
    result1.res should contain theSameElementsAs result2.res
    result1.ext should contain theSameElementsAs result2.ext
  }

  // OWLObjectOneOf
  class_OWLObjectOneOf.toString should "be converted into a RDFoxRuleShards" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectOneOf.accept(visitor)
    result shouldBe a[RDFoxRuleShards]
  }

  // it should "be converted into a single <owl:sameAs> TupleTableAtom" in {
  //   val visitor = RDFoxClassExprConverter(term_x)
  //   val result = class_OWLObjectOneOf.accept(visitor)
  //   result.res.loneElement should (be (a [TupleTableAtom]) and have ('tupleTableName (pred_sameAs)))
  // }

  it should "have an empty extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectOneOf.accept(visitor)
    result.ext shouldBe empty
  }

  // OWLObjectSomeValuesFrom
  (class_OWLObjectSomeValuesFrom.toString ++ " w/o skolemization") should
    "be converted into a RDFoxRuleShards" in {
      val visitor = RDFoxClassExprConverter(term_x)
      val result = class_OWLObjectSomeValuesFrom.accept(visitor)
      result shouldBe a[RDFoxRuleShards]
    }

  it should "have two TupleTableAtoms in its result list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    exactly(2, result.res) should (be(an[TupleTableAtom])
      //and have('numberOfArguments (3))
    )
  }

  it should "have an empty extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
    result.ext shouldBe empty
  }

  //(class_OWLObjectSomeValuesFrom.toString ++ " w/ skolemization") should
  //  "be converted into a RDFoxRuleShards" in {
  //    val skolem = Standard(class_OWLObjectSomeValuesFrom)
  //    val visitor = RDFoxClassExprConverter(term_x, List(), skolem)
  //    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
  //    result shouldBe a[RDFoxRuleShards]
  //  }

  //it should "have exactly two TupleTableAtoms in its result list" in {
  //  val skolem = Standard(class_OWLObjectSomeValuesFrom)
  //  val visitor = RDFoxClassExprConverter(term_x, List(), skolem)
  //  val result = class_OWLObjectSomeValuesFrom.accept(visitor)
  //  exactly(2, result.res) should (be(an[TupleTableAtom])
  //    //and have('numberOfArguments (3))
  //  )
  //}

  //it should "should have a single SKOLEM call in the extension list" in {
  //  val skolem = Standard(class_OWLObjectSomeValuesFrom)
  //  val visitor = RDFoxClassExprConverter(term_x, List(), skolem)
  //  val result = class_OWLObjectSomeValuesFrom.accept(visitor)
  //  result.ext.loneElement shouldBe a[BindAtom]
  //  val builtin = result.ext.head.asInstanceOf[BindAtom].getExpression
  //  builtin should (be(a[FunctionCall]) and have(
  //    'functionName ("SKOLEM")
  //  ))
  //}

  //(class_OWLObjectSomeValuesFrom.toString ++ " w/ constant skolemization") should
  //  "be converted into a RDFoxRuleShards" in {
  //    val skolem = Constant(class_OWLObjectSomeValuesFrom)
  //    val visitor = RDFoxClassExprConverter(term_x, List(), skolem)
  //    val result = class_OWLObjectSomeValuesFrom.accept(visitor)
  //    result shouldBe a[RDFoxRuleShards]
  //  }

  //it should "have exactly two TupleTableAtoms in its result list" in {
  //  val skolem = Constant(class_OWLObjectSomeValuesFrom)
  //  val visitor = RDFoxClassExprConverter(term_x, List(), skolem)
  //  val result = class_OWLObjectSomeValuesFrom.accept(visitor)
  //  exactly(2, result.res) should (be(an[TupleTableAtom])
  //    //and have('numberOfArguments (3))
  //  )
  //}

  //it should "have an empty extension list" in {
  //  val skolem = Constant(class_OWLObjectSomeValuesFrom)
  //  val visitor = RDFoxClassExprConverter(term_x, List(), skolem)
  //  val result = class_OWLObjectSomeValuesFrom.accept(visitor)
  //  result.ext shouldBe empty
  //}

  // OWLObjectMaxCardinalityImpl
  class_OWLObjectMaxCardinality.toString should
    "be converted into a RDFoxRuleShards" in {
      val visitor = RDFoxClassExprConverter(term_x)
      val result = class_OWLObjectMaxCardinality.accept(visitor)
      result shouldBe a[RDFoxRuleShards]
    }

  // it should "have a single <owl:sameAs> TupleTableAtom in the result list" in {
  //   val visitor = RDFoxClassExprConverter(term_x)
  //   val result = class_OWLObjectMaxCardinality.accept(visitor)
  //   result.res.loneElement should (be(an[TupleTableAtom]) and have(
  //     'tupleTableName (pred_sameAs)
  //   ))
  // }

  it should "have 4 TupleTableAtoms in its extension list" in {
    val visitor = RDFoxClassExprConverter(term_x)
    val result = class_OWLObjectMaxCardinality.accept(visitor)
    exactly(4, result.ext) should (be(an[TupleTableAtom])
      //and have('numberOfArguments (3))
    )
  }

} // class OWLClassSpec
