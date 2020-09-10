package rsacomb

import java.util.ArrayList
import org.scalatest.{FlatSpec, Matchers, LoneElement}

import uk.ac.manchester.cs.owl.owlapi.{OWLSubClassOfAxiomImpl}
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
import org.semanticweb.owlapi.model.{OWLAxiom}

import tech.oxfordsemantic.jrdfox.logic.{Rule, BindAtom, BuiltinFunctionCall}
import tech.oxfordsemantic.jrdfox.logic.{
  Atom,
  TupleTableName,
  Term,
  Variable,
  Literal,
  Datatype
}

import org.semanticweb.owlapi.model.{IRI => OWLIRI}
import tech.oxfordsemantic.jrdfox.logic.{IRI => RDFIRI}

object OWLAxiomSpec {

  // IRI
  val iri_Professor = OWLIRI.create("univ:Professor")
  val iri_Female = OWLIRI.create("std:Female")
  val iri_Student = OWLIRI.create("univ:Student")
  val iri_PartTimeStudent = OWLIRI.create("univ:PartTimeStudent")
  val iri_Worker = OWLIRI.create("univ:Worker")
  val iri_alice = OWLIRI.create("univ:alice")
  val iri_supervises = OWLIRI.create("univ:supervises")
  val iri_hasSupervisor = OWLIRI.create("univ:hasSupervisor")
  val iri_sameAs = OWLIRI.create("owl:sameAs")

  // RDFox Terms
  val term_x = Variable.create("x")
  val term_y = Variable.create("y")
  val term_z = Variable.create("z")
  val term_c1 = RSA.internal("c_1")
  val term_c2 = RSA.internal("c_2")
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
  val class_PartTimeStudent = new OWLClassImpl(iri_PartTimeStudent)
  val class_Worker = new OWLClassImpl(iri_Worker)
  val class_OWLClass = class_Professor
  // Class Conjunction corresponding to
  //
  //    Student ∧ Worker
  //
  val class_OWLObjectIntersectionOf =
    new OWLObjectIntersectionOfImpl(
      class_Student,
      class_Worker
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
  ): List[Rule] = {
    axiom.accept(RDFoxAxiomConverter(term, skolem))
  }

} // object OWLAxiomSpec

class OWLAxiomSpec extends FlatSpec with Matchers with LoneElement {

  // Import required data
  import OWLAxiomSpec._
  // Implicit convertion from IRI in OWLAPI to IRI in JRDFox
  import RDFoxUtil._

  // OWLSubClassOfAxiom #1
  axiom_OWLSubClassOf1.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf1, term_x)
    result.loneElement shouldBe a[Rule]
  }

  it should "contain a conjuction of atoms (Student[?x],Worker[?x]) in the body of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf1, term_x)
    val body = List(
      Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Student),
      Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Worker)
    )
    result.loneElement.getBody should contain theSameElementsAs body
  }

  it should "contain a single atom (PartTimeStudent[?x]) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf1, term_x)
    val head = Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_PartTimeStudent)
    result.loneElement.getHead.loneElement should be(head)
  }

  // OWLSubClassOfAxiom #2 (w/ constant skolemization)
  (axiom_OWLSubClassOf2.toString + "\n(w/ constant skolemization)") should
    "be converted into a singleton List[Rule]" in {
    val skolem = SkolemStrategy.Constant(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    result.loneElement shouldBe a[Rule]
  }

  it should "contain a single atom (Student[?x]) in the body of the rule" in {
    val skolem = SkolemStrategy.Constant(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    val body =
      Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Student.getIRIString)
    result.loneElement.getBody.loneElement should equal(body)
  }

  // it should "contain a conjuction of atoms (hasSupervisor[?x,?c],Professor[?c]) in the head of the rule" in {
  //   val skolem = SkolemStrategy.Constant(axiom_OWLSubClassOf2.toString)
  //   val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
  //   val term_c = RSA.internal(skolem.const.getIRI)
  //   val head = List(
  //     Atom.rdf(term_x, iri_hasSupervisor, term_c),
  //     Atom.rdf(term_c, RDFIRI.RDF_TYPE, iri_Professor)
  //   )
  //   result.loneElement.getHead should contain theSameElementsAs (head)
  // }

  // OWLSubClassOfAxiom #2 (w/ skolemization)
  (axiom_OWLSubClassOf2.toString + "\n(w/ skolemization)") should
    "be converted into a singleton List[Rule]" in {
    val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    result.loneElement shouldBe a[Rule]
  }

  it should "contain an atom (Student[?x]) in the body of the rule" in {
    val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
    val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
    val body =
      Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Student)
    result.loneElement.getBody should contain(body)
  }

  // it should "contain a built-in function call (BIND(?y,SKOLEM(?f,?x))) in the body of the rule" in {
  //   val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
  //   val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
  //   val call =
  //     BindAtom.create(BuiltinFunctionCall.create("SKOLEM", term_x), term_y)
  //   result.loneElement.getBody should contain(call)
  // }

  // it should "contain a conjuction of atom (hasSupervisor[?x,?y],Professor[?y]) in the head of the rule" in {
  //   val skolem = SkolemStrategy.Standard(axiom_OWLSubClassOf2.toString)
  //   val result = convertAxiom(axiom_OWLSubClassOf2, term_x, skolem)
  //   val head = List(
  //     Atom.rdf(term_x, iri_hasSupervisor, term_y),
  //     Atom.rdf(term_y, RDFIRI.RDF_TYPE, iri_Professor)
  //   )
  //   result.loneElement.getHead should contain theSameElementsAs head
  // }

  // OWLSubClassOfAxiom #3
  axiom_OWLSubClassOf3.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf3, term_x)
    result.loneElement shouldBe a[Rule]
  }

  // it should "contain a conjunction of atoms (hasSupervisor[?x,?y],Professor[?y]) in the body of the rule" in {
  //   val result = convertAxiom(axiom_OWLSubClassOf3, term_x)
  //   val body = List(
  //     Atom.rdf(term_x, iri_hasSupervisor, term_y),
  //     Atom.rdf(term_y, RDFIRI.RDF_TYPE, iri_Professor)
  //   )
  //   result.loneElement.getBody should contain theSameElementsAs body
  // }

  it should "contain a single atom (Student[?x]) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf3, term_x)
    val head =
      Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Student)
    result.loneElement.getHead.loneElement should be(head)
  }

  // OWLSubClassOfAxiom #4
  axiom_OWLSubClassOf4.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf4, term_x)
    result.loneElement shouldBe a[Rule]
  }

  it should "contain a single atoms (Student[?x]) in the body of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf4, term_x)
    val body =
      Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Student)
    result.loneElement.getBody.loneElement should be(body)
  }

  it should "contain a single atom (sameAs[?x,alice])) in the head of the rule" in {
    val result = convertAxiom(axiom_OWLSubClassOf4, term_x)
    val head = Atom.rdf(term_x, RDFIRI.SAME_AS, term_alice)
    result.loneElement.getHead.loneElement should be(head)
  }

  // OWLSubClassOfAxiom #5
  axiom_OWLSubClassOf5.toString should "be converted into a singleton List[Rule]" in {
    val result = convertAxiom(axiom_OWLSubClassOf5, term_x)
    result.loneElement shouldBe a[Rule]
  }

  // it should "contain a conjunction of atoms (...) in the body of the rule" in {
  //   val result = convertAxiom(axiom_OWLSubClassOf5, term_x)
  //   val body = List(
  //     Atom.rdf(term_x, RDFIRI.RDF_TYPE, iri_Student),
  //     Atom.rdf(term_x, iri_hasSupervisor, term_y),
  //     Atom.rdf(term_y, RDFIRI.RDF_TYPE, iri_Professor),
  //     Atom.rdf(term_x, iri_hasSupervisor, term_z),
  //     Atom.rdf(term_z, RDFIRI.RDF_TYPE, iri_Professor)
  //   )
  //   result.loneElement.getBody should contain theSameElementsAs body
  // }

  // it should "contain a single atom (sameAs[?x,?z])) in the head of the rule" in {
  //   val result = convertAxiom(axiom_OWLSubClassOf5, term_x)
  //   val head = Atom.rdf(term_y, RDFIRI.SAME_AS, term_z)
  //   result.loneElement.getHead.loneElement should be(head)
  // }

} // class OWLAxiomSpec
