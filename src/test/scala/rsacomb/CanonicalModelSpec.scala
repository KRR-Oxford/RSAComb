package rsacomb

import java.io.File
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.semanticweb.owlapi.model._
import uk.ac.manchester.cs.owl.owlapi._
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer

import tech.oxfordsemantic.jrdfox.logic.{Rule, Variable}

import scala.collection.JavaConverters._

import rsacomb.RSA._
import rsacomb.RDFoxUtil._

object Ontology1_CanonicalModelSpec {

  /* Renderer to display OWL Axioms with DL syntax*/
  val renderer = new DLSyntaxObjectRenderer()

  val ontology_path: File = new File("examples/example1.owl")
  val ontology = RSA.loadOntology(ontology_path)
  val program = ontology.canonicalModel

  val roleR = new OWLObjectPropertyImpl(RSA.base("R"))
  val roleS = new OWLObjectPropertyImpl(RSA.base("S"))
  val roleT = new OWLObjectPropertyImpl(RSA.base("T"))
  val roleR_inv = roleR.getInverseProperty()
  val roleS_inv = roleS.getInverseProperty()
  val roleT_inv = roleT.getInverseProperty()

  val AsubClassOfD = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("A")),
    new OWLClassImpl(RSA.base("D")),
    Seq().asJava
  )

  val DsomeValuesFromRB = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("D")),
    new OWLObjectSomeValuesFromImpl(
      roleR,
      new OWLClassImpl(RSA.base("B"))
    ),
    Seq().asJava
  )

  val BsomeValuesFromSD = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("B")),
    new OWLObjectSomeValuesFromImpl(
      roleS,
      new OWLClassImpl(RSA.base("D"))
    ),
    Seq().asJava
  )

} // object OWLAxiomSpec

class Ontology1_CanonicalModelSpec
    extends AnyFlatSpec
    with Matchers
    with LoneElement {

  import Ontology1_CanonicalModelSpec._

  "The program generated from Example #1" should "not be empty" in {
    program should not be empty
  }

  renderer.render(AsubClassOfD) should "be converted into a single Rule" in {
    val varX = Variable.create("X")
    val visitor = ProgramGenerator(ontology, varX)
    val rules = AsubClassOfD.accept(visitor)
    rules.loneElement shouldBe a[Rule]
  }

  // Role R //

  renderer.render(roleR) should "be safe" in {
    ontology.unsafeRoles should not contain roleR
  }

  it should "have 3 elements in its conflict set" in {
    ontology.confl(roleR) should have size 3
  }

  it should "contain S in its conflict set" in {
    ontology.confl(roleR) should contain(roleS)
  }

  it should "contain T in its conflict set" in {
    ontology.confl(roleR) should contain(roleT)
  }

  it should ("contain " + renderer.render(
    roleR_inv
  ) + " in its conflict set") in {
    ontology.confl(roleR) should contain(roleR_inv)
  }

  // Role S //

  renderer.render(roleS) should "be safe" in {
    ontology.unsafeRoles should not contain roleS
  }

  it should "have 3 elements in its conflict set" in {
    ontology.confl(roleS) should have size 3
  }

  it should "contain R in its conflict set" in {
    ontology.confl(roleS) should contain(roleR)
  }

  it should ("contain " + renderer.render(
    roleS_inv
  ) + " in its conflict set") in {
    ontology.confl(roleS) should contain(roleS_inv)
  }

  it should ("contain " + renderer.render(
    roleT_inv
  ) + " in its conflict set") in {
    ontology.confl(roleS) should contain(roleT_inv)
  }

  renderer.render(roleS_inv) should "be unsafe" in {
    ontology.unsafeRoles should contain(roleS_inv)
  }

  it should ("contain " + renderer.render(
    roleR_inv
  ) + " in its conflict set") in {
    ontology.confl(roleS_inv) should contain(roleR_inv)
  }

  renderer.render(
    DsomeValuesFromRB
  ) should "have a sigleton 'cycle' set" in {
    // Using `hashCode` we are assuming (B,S,D) < (D,R,B)
    val ind = RSA.internal("v1_" ++ BsomeValuesFromSD.hashCode.toString())
    ontology.cycle(DsomeValuesFromRB).loneElement shouldBe ind
  }

  it should "produce 5 rules" in {
    // Rule 1 provides 1 rule (split in 2) + 1 fact
    // Rule 2 provides 0 rules
    // Rule 3 provides 1 rule (split in 2)
    val varX = Variable.create("X")
    val visitor = ProgramGenerator(ontology, varX)
    val rules = DsomeValuesFromRB.accept(visitor)
    rules should have length 5
  }

  renderer.render(
    BsomeValuesFromSD
  ) should "have a sigleton 'cycle' set" in {
    // Using `hashCode` we are assuming (B,S,D) < (D,R,B)
    val ind = RSA.internal("v0_" ++ DsomeValuesFromRB.hashCode.toString())
    ontology.cycle(BsomeValuesFromSD).loneElement shouldBe ind
  }

  it should "produce 5 rules" ignore {
    // Rule 1 provides 1 rule (split in 2) + 1 fact
    // Rule 2 provides 0 rules
    // Rule 3 provides 1 rule (split in 2)
    val varX = Variable.create("X")
    val visitor = ProgramGenerator(ontology, varX)
    val rules = BsomeValuesFromSD.accept(visitor)
    rules should have length 5
  }

} // class OWLAxiomSpec
