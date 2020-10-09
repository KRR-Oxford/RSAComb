package rsacomb

import java.io.File
import org.scalatest.{FlatSpec, Matchers, LoneElement}

import org.semanticweb.owlapi.model._
import uk.ac.manchester.cs.owl.owlapi._

import tech.oxfordsemantic.jrdfox.logic.{Rule, Variable}

import scala.collection.JavaConverters._

import rsacomb.RSA._
import rsacomb.RDFoxUtil._

object CanonicalModelSpec {

  val ontology1_path: File = new File("examples/example1.owl")
  val ontology1 = RSA.loadOntology(ontology1_path)
  val program1 = ontology1.canonicalModel

  val axiom1 = new OWLSubClassOfAxiomImpl(
    new OWLClassImpl(RSA.base("A")),
    new OWLClassImpl(RSA.base("D")),
    Seq().asJava
  )

} // object OWLAxiomSpec

class CanonicalModelSpec extends FlatSpec with Matchers with LoneElement {

  import CanonicalModelSpec._

  // Example 1
  "The program generated from Example1" should "not be empty" in {
    program1 should not be empty
  }

  axiom1.toString should "be converted into a single Rule" in {
    val varX = Variable.create("X")
    val visitor = ProgramGenerator(ontology1, varX)
    val rules = axiom1.accept(visitor)
    rules.loneElement shouldBe a[Rule]
  }

} // class OWLAxiomSpec
