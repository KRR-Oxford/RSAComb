package rsacomb

import java.util.ArrayList
import org.scalatest.{FlatSpec, Matchers, LoneElement}

import uk.ac.manchester.cs.owl.owlapi.{OWLSubClassOfAxiomImpl}
import uk.ac.manchester.cs.owl.owlapi.{OWLClassImpl, OWLObjectSomeValuesFromImpl, OWLObjectIntersectionOfImpl, OWLObjectOneOfImpl, OWLObjectAllValuesFromImpl, OWLObjectMaxCardinalityImpl, OWLNamedIndividualImpl}
import uk.ac.manchester.cs.owl.owlapi.{OWLObjectPropertyImpl}
import org.semanticweb.owlapi.model.{OWLAxiom,IRI}

import tech.oxfordsemantic.jrdfox.logic.{Rule,Bind,BuiltinFunctionCall}
import tech.oxfordsemantic.jrdfox.logic.{Atom, Predicate, Term, Variable, Individual}

object OWLAxiomSpec {

  // IRI
  val iri_Professor = IRI.create("univ:Professor")
  val iri_Female    = IRI.create("std:Female")
  val iri_Student   = IRI.create("univ:Student")
  val iri_PartTimeStudent = IRI.create("univ:PartTimeStudent")
  val iri_Worker    = IRI.create("univ:Worker")
  val iri_alice     = IRI.create("univ:alice")
  val iri_supervises    = IRI.create("univ:supervises")
  val iri_hasSupervisor = IRI.create("univ:hasSupervisor")
  val iri_sameAs = IRI.create("owl:sameAs")

  // RDFox Terms
  val term_x = Variable.create("x")
  val term_y = Variable.create("y")
  val term_z = Variable.create("z")
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
  val class_PartTimeStudent   = new OWLClassImpl(iri_PartTimeStudent)
  val class_Worker    = new OWLClassImpl(iri_Worker)
  val class_OWLClass = class_Professor
  // Class Conjunction corresponding to
  //
  //    Student ∧ Worker
  //
  val class_OWLObjectIntersectionOf = 
    new OWLObjectIntersectionOfImpl(
      class_Student, class_Worker
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
  //    ∃ hasSupervisor.Professor
  //
  val class_OWLObjectSomeValuesFrom =
    new OWLObjectSomeValuesFromImpl(
      new OWLObjectPropertyImpl(iri_hasSupervisor),
      class_Professor
    )
  // Object Max Cardinality Restriction corresponding to
  //
  //    ≤ 1 hasSupervisor . Professor
  val class_OWLObjectMaxCardinality =
    new OWLObjectMaxCardinalityImpl(
      new OWLObjectPropertyImpl(iri_hasSupervisor),
      1,
      class_Professor
    )

  // OWL Axioms
  // Axiom SubClassOf corresponding to
  //
  //    Student ∧ Worker -> PartTimeStudent
  //
  val axiom_OWLSubClassOf1 = 
    new OWLSubClassOfAxiomImpl(
      class_OWLObjectIntersectionOf,
      class_PartTimeStudent,
      new ArrayList()
    )

  // Axiom SubClassOf corresponding to
  //
  //    Student -> ∃ hasSupervisor.Professor
  //
  val axiom_OWLSubClassOf2 =
    new OWLSubClassOfAxiomImpl(
      class_Student,
      class_OWLObjectSomeValuesFrom,
      new ArrayList()
    )

  // Axiom SubClassOf corresponding to
  //
  //    ∃ hasSupervisor.Professor -> Student
  //
  val axiom_OWLSubClassOf3 =
    new OWLSubClassOfAxiomImpl(
      class_OWLObjectSomeValuesFrom,
      class_Student,
      new ArrayList()
    )

  // Axiom SubClassOf corresponding to
  //
  //    Student -> { alice }
  //
  val axiom_OWLSubClassOf4 =
    new OWLSubClassOfAxiomImpl(
      class_Student,
      class_OWLObjectOneOf,
      new ArrayList()
    )

  // Axiom SubClassOf corresponding to
  //
  //    Student -> ≤1 hasSupervisor.Professor
  //
  val axiom_OWLSubClassOf5 =
    new OWLSubClassOfAxiomImpl(
      class_Student,
      class_OWLObjectMaxCardinality,
      new ArrayList()
    )

  def convertAxiom(
    axiom: OWLAxiom,
    term: Term,
    skolem: SkolemStrategy = SkolemStrategy.None
  ) : List[Rule] = {
    axiom.accept(RDFoxAxiomConverter(term,skolem))
  }

} // object OWLAxiomSpec

class OWLAxiomSpec
  extends FlatSpec with Matchers with LoneElement
{
  // Import required data
  import OWLAxiomSpec._

  // OWLSubClassOfAxiom #1
  axiom_OWLSubClassOf1.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf1,term_x)
    result.loneElement shouldBe a [Rule]
  }

  it should "contain a conjuction of atoms (Student[?x],Worker[?x]) in the body of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf1,term_x)
    val body = List(
      Atom.create(Predicate.create(iri_Student.getIRIString),term_x),
      Atom.create(Predicate.create(iri_Worker.getIRIString),term_x)
    )
    result.loneElement.getBody should contain theSameElementsAs body
  }

  it should "contain a single atom (PartTimeStudent[?x]) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf1,term_x)
    val head = Atom.create(Predicate.create(iri_PartTimeStudent.getIRIString),term_x)
    result.loneElement.getHead.loneElement should be (head)
  }

  // OWLSubClassOfAxiom #2 (w/ constant skolemization)
  (axiom_OWLSubClassOf2.toString + "\n(w/ constant skolemization)") should
    "be converted into a singleton List[Rule]" in
  {
    val skolem = SkolemStrategy.Constant(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2,term_x,skolem)
    result.loneElement shouldBe a [Rule]
  }

  it should "contain a single atom (Student[?x]) in the body of the rule" in {
    val skolem = SkolemStrategy.Constant(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2,term_x,skolem)
    val body = Atom.create(Predicate.create(iri_Student.getIRIString),term_x)
    result.loneElement.getBody.loneElement should equal (body)
  }

  it should "contain a conjuction of atoms (hasSupervisor[?x,?c],Professor[?c]) in the head of the rule" in {
    val skolem = SkolemStrategy.Constant(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2,term_x,skolem)
    val term_c = Individual.create(skolem.const)
    val head = List(
      Atom.create(Predicate.create(iri_hasSupervisor.getIRIString),term_x,term_c),
      Atom.create(Predicate.create(iri_Professor.getIRIString),term_c)
    )
    result.loneElement.getHead should contain theSameElementsAs (head)
  }

  // OWLSubClassOfAxiom #2 (w/ skolemization)
  (axiom_OWLSubClassOf2.toString + "\n(w/ skolemization)") should
    "be converted into a singleton List[Rule]" in
  {
    val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    result.loneElement shouldBe a [Rule]
  }

  it should "contain an atom (Student[?x]) in the body of the rule" in {
    val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    val body = Atom.create(Predicate.create(iri_Student.getIRIString),term_x)
    result.loneElement.getBody should contain (body)
  }

  it should "contain a built-in function call (BIND(?y,SKOLEM(?f,?x))) in the body of the rule" in {
    val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    val call = Bind.create(BuiltinFunctionCall.create("SKOLEM",term_x),term_y)
    result.loneElement.getBody should contain (call)
  }

  it should "contain a conjuction of atom (hasSupervisor[?x,?y],Professor[?y]) in the head of the rule" in {
    val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    val head = List(
      Atom.create(Predicate.create(iri_hasSupervisor.getIRIString),term_x,term_y),
      Atom.create(Predicate.create(iri_Professor.getIRIString),term_y)
    )
    result.loneElement.getHead should contain theSameElementsAs head
  }

  // OWLSubClassOfAxiom #3
  axiom_OWLSubClassOf3.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf3,term_x)
    result.loneElement shouldBe a [Rule]
  }

  it should "contain a conjunction of atoms (hasSupervisor[?x,?y],Professor[?y]) in the body of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf3,term_x)
    val body = List(
      Atom.create(Predicate.create(iri_hasSupervisor.getIRIString),term_x,term_y),
      Atom.create(Predicate.create(iri_Professor.getIRIString),term_y)
    )
    result.loneElement.getBody should contain theSameElementsAs body
  }

  it should "contain a single atom (Student[?x]) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf3, term_x)
    val head = Atom.create(Predicate.create(iri_Student.getIRIString),term_x)
    result.loneElement.getHead.loneElement should be (head)
  }

  // OWLSubClassOfAxiom #4
  axiom_OWLSubClassOf4.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf4,term_x)
    result.loneElement shouldBe a [Rule]
  }

  it should "contain a single atoms (Student[?x]) in the body of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf4,term_x)
    val body = Atom.create(Predicate.create(iri_Student.getIRIString),term_x)
    result.loneElement.getBody.loneElement should be (body)
  }

  it should "contain a single atom (sameAs[?x,alice])) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf4, term_x)
    val head = Atom.create(Predicate.create(iri_sameAs.getIRIString),term_x,term_alice)
    result.loneElement.getHead.loneElement should be (head)
  }

  // OWLSubClassOfAxiom #5
  axiom_OWLSubClassOf5.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf5,term_x)
    result.loneElement shouldBe a [Rule]
  }

  it should "contain a conjunction of atoms (...) in the body of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf5,term_x)
    val body = List(
      Atom.create(Predicate.create(iri_Student.getIRIString),term_x),
      Atom.create(Predicate.create(iri_hasSupervisor.getIRIString),term_x,term_y),
      Atom.create(Predicate.create(iri_Professor.getIRIString),term_y),
      Atom.create(Predicate.create(iri_hasSupervisor.getIRIString),term_x,term_z),
      Atom.create(Predicate.create(iri_Professor.getIRIString),term_z)
    )
    result.loneElement.getBody should contain theSameElementsAs body
  }

  it should "contain a single atom (sameAs[?x,?z])) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf5, term_x)
    val head = Atom.create(Predicate.create(iri_sameAs.getIRIString),term_y,term_z)
    result.loneElement.getHead.loneElement should be (head)
  }

} // class OWLAxiomSpec
