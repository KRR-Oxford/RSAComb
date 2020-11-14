package rsacomb

import java.io.File
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.semanticweb.owlapi.model._
import uk.ac.manchester.cs.owl.owlapi._
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer

import tech.oxfordsemantic.jrdfox.logic.datalog.Rule
import tech.oxfordsemantic.jrdfox.logic.expression.Variable

import scala.collection.JavaConverters._

import rsacomb.RSA._
import rsacomb.RDFoxUtil._

object Ontology1_CanonicalModelSpec {

  /* Renderer to display OWL Axioms with DL syntax*/
  val renderer = new DLSyntaxObjectRenderer()

  val ontology_path: File = new File("examples/example1.ttl")
  val ontology: RSAOntology = RSA.loadOntology(ontology_path)
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

  val AsomeValuesFromSiC = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("A")),
    new OWLObjectSomeValuesFromImpl(
      roleS_inv,
      new OWLClassImpl(RSA.base("C"))
    ),
    Seq().asJava
  )

  val SsubPropertyOfT = new OWLSubObjectPropertyOfAxiomImpl(
    new OWLObjectPropertyImpl(RSA.base("S")),
    new OWLObjectPropertyImpl(RSA.base("T")),
    Seq().asJava
  )

}

class Ontology1_CanonicalModelSpec
    extends AnyFlatSpec
    with Matchers
    with LoneElement {

  import Ontology1_CanonicalModelSpec._

  "The program generated from Example #1" should "not be empty" in {
    program.rules should not be empty
  }

  renderer.render(AsubClassOfD) should "be converted into a single Rule" in {
    val varX = Variable.create("X")
    val visitor = program.ProgramGenerator
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

  // S⁻

  renderer.render(roleS_inv) should "be unsafe" in {
    ontology.unsafeRoles should contain(roleS_inv)
  }

  renderer.render(
    AsomeValuesFromSiC
  ) should "produce 1 rule" in {
    val varX = Variable.create("X")
    val visitor = program.ProgramGenerator
    val rules = AsomeValuesFromSiC.accept(visitor)
    rules should have length 1
  }

  renderer.render(
    DsomeValuesFromRB
  ) should "have a 'cycle' set of 48 elements" in {
    // Cycle introduces a new constant for each possible triple (the
    // order among triples is total). In this example there are 4
    // concept names and R has 3 safe roles in its conflict set (S, T,
    // Inv(R)). Triples are
    //    (concept, role, concept)
    // and hence we have 4*3*4=48 new constants introduced.
    ontology.cycle(DsomeValuesFromRB) should have size 48
  }

  it should "produce 5 rules" in {
    // Rule 1 provides 1 rule (split in 2) + 48 fact
    // Rule 2 provides 0 rules
    // Rule 3 provides 48 rule (split in 2)
    // Then (1*2 + 48) + (0) + (48*2) = 146
    val varX = Variable.create("X")
    val visitor = program.ProgramGenerator
    val rules = DsomeValuesFromRB.accept(visitor)
    rules should have length 146
  }

  renderer.render(
    BsomeValuesFromSD
  ) should "have a 'cycle' set of 32 elements" in {
    // Cycle introduces a new constant for each possible triple (the
    // order among triples is total). In this example there are 4
    // concept names and S has 2 safe roles in its conflict set (R,
    // Inv(T)). Triples are
    //    (concept, role, concept)
    // and hence we have 4*2*4=32 new constants introduced.
    ontology.cycle(BsomeValuesFromSD) should have size 32
  }

  it should "produce 5 rules" in {
    // Rule 1 provides 1 rule (split in 2) + 32 fact
    // Rule 2 provides 0 rules
    // Rule 3 provides 32 rule (split in 2)
    // Then (1*2 + 32) + (0) + (32*2) = 98
    val varX = Variable.create("X")
    val visitor = program.ProgramGenerator
    val rules = DsomeValuesFromRB.accept(visitor)
    rules should have length 146
  }

  renderer.render(
    SsubPropertyOfT
  ) should "produce 2 rules" in {
    val varX = Variable.create("X")
    val visitor = program.ProgramGenerator
    val rules = SsubPropertyOfT.accept(visitor)
    rules should have length 2
  }

}

object Ontology2_CanonicalModelSpec {

  /* Renderer to display OWL Axioms with DL syntax*/
  val renderer = new DLSyntaxObjectRenderer()

  val ontology_path: File = new File("examples/example2.owl")
  val ontology: RSAOntology = RSA.loadOntology(ontology_path)
  val program = ontology.canonicalModel

  val roleR = new OWLObjectPropertyImpl(RSA.base("R"))
  val roleS = new OWLObjectPropertyImpl(RSA.base("S"))
  val roleT = new OWLObjectPropertyImpl(RSA.base("T"))
  val roleP = new OWLObjectPropertyImpl(RSA.base("P"))
  val roleR_inv = roleR.getInverseProperty()
  val roleS_inv = roleS.getInverseProperty()
  val roleT_inv = roleT.getInverseProperty()
  val roleP_inv = roleP.getInverseProperty()

  val AsomeValuesFromRB = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("A")),
    new OWLObjectSomeValuesFromImpl(
      roleR,
      new OWLClassImpl(RSA.base("B"))
    ),
    Seq().asJava
  )

  val BsomeValuesFromSC = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("B")),
    new OWLObjectSomeValuesFromImpl(
      roleS,
      new OWLClassImpl(RSA.base("C"))
    ),
    Seq().asJava
  )

  val CsomeValuesFromTD = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("C")),
    new OWLObjectSomeValuesFromImpl(
      roleT,
      new OWLClassImpl(RSA.base("D"))
    ),
    Seq().asJava
  )

  val DsomeValuesFromPA = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("D")),
    new OWLObjectSomeValuesFromImpl(
      roleP,
      new OWLClassImpl(RSA.base("A"))
    ),
    Seq().asJava
  )

}

class Ontology2_CanonicalModelSpec
    extends AnyFlatSpec
    with Matchers
    with LoneElement {

  import Ontology2_CanonicalModelSpec._

  "The program generated from Example #1" should "not be empty" in {
    program.rules should not be empty
  }

  // Role R //

  renderer.render(roleR) should "be unsafe" in {
    ontology.unsafeRoles should contain(roleR)
  }

  it should "have only its inverse in its conflict set" in {
    ontology.confl(roleR).loneElement shouldBe roleR_inv
  }

  // Role S //

  renderer.render(roleS) should "be unsafe" in {
    ontology.unsafeRoles should contain(roleS)
  }

  it should "have only its inverse in its conflict set" in {
    ontology.confl(roleS).loneElement shouldBe roleS_inv
  }

  // Role T //

  renderer.render(roleT) should "be unsafe" in {
    ontology.unsafeRoles should contain(roleT)
  }

  it should "have only its inverse in its conflict set" in {
    ontology.confl(roleT).loneElement shouldBe roleT_inv
  }

  // Role P //

  renderer.render(roleP) should "be unsafe" in {
    ontology.unsafeRoles should contain(roleP)
  }

  it should "have only its inverse in its conflict set" in {
    ontology.confl(roleP).loneElement shouldBe roleP_inv
  }

  // A ⊑ ∃ R.B

  renderer.render(
    AsomeValuesFromRB
  ) should "produce 1 rule" in {
    val visitor = program.ProgramGenerator
    val rules = AsomeValuesFromRB.accept(visitor)
    rules should have length 1
  }

  // B ⊑ ∃ S.C

  renderer.render(
    BsomeValuesFromSC
  ) should "produce 1 rule" in {
    val visitor = program.ProgramGenerator
    val rules = BsomeValuesFromSC.accept(visitor)
    rules should have length 1
  }

  // C ⊑ ∃ T.D

  renderer.render(
    CsomeValuesFromTD
  ) should "produce 1 rule" in {
    val visitor = program.ProgramGenerator
    val rules = CsomeValuesFromTD.accept(visitor)
    rules should have length 1
  }

  // D ⊑ ∃ P.A

  renderer.render(
    DsomeValuesFromPA
  ) should "produce 1 rule" in {
    val visitor = program.ProgramGenerator
    val rules = DsomeValuesFromPA.accept(visitor)
    rules should have length 1
  }

}
