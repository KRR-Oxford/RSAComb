package rsacomb

import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntologyManager

import tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom
import tech.oxfordsemantic.jrdfox.logic.expression.{Variable, IRI}
import uk.ac.ox.cs.rsacomb.converter.RDFoxConverter
import uk.ac.ox.cs.rsacomb.suffix.{Empty, Forward, Backward, Inverse}
import uk.ac.ox.cs.rsacomb.converter.SkolemStrategy

object RDFoxConverterSpec {

  val manager = OWLManager.createOWLOntologyManager()
  val factory = manager.getOWLDataFactory

  val term0 = Variable.create("X")
  val term1 = Variable.create("Y")
  val iriString0 = "http://example.com/rsacomb/iri0"
  val iriString1 = "http://example.com/rsacomb/iri1"
  val iriString2 = "http://example.com/rsacomb/iri2"
  val suffixes = Seq(
    Empty,
    Forward,
    Backward,
    Inverse,
    Forward + Inverse,
    Backward + Inverse
  )
}

class RDFoxConverterSpec
    extends AnyFlatSpec
    with Matchers
    with LoneElement
    with RDFoxConverter {

  import RDFoxConverterSpec._

  "A class name" should "be converted into a single atom" in {
    val cls = factory.getOWLClass(iriString0)
    val atom = TupleTableAtom.rdf(term0, IRI.RDF_TYPE, IRI.create(iriString0))
    val (res, ext) =
      convert(cls, term0, List(), SkolemStrategy.None, Empty)
    res.loneElement shouldEqual atom
    ext shouldBe empty
  }

  "A intersection of classes" should "be converted into the union of the conversion of the classes" in {
    val cls0 = factory.getOWLClass(iriString0)
    val cls1 = factory.getOWLClass(iriString1)
    val cls2 = factory.getOWLClass(iriString2)
    val conj = factory.getOWLObjectIntersectionOf(cls0, cls1, cls2)
    val (res0, ext0) =
      convert(cls0, term0, List(), SkolemStrategy.None, Empty)
    val (res1, ext1) =
      convert(cls1, term0, List(), SkolemStrategy.None, Empty)
    val (res2, ext2) =
      convert(cls2, term0, List(), SkolemStrategy.None, Empty)
    val (res, ext) =
      convert(conj, term0, List(), SkolemStrategy.None, Empty)
    res should contain theSameElementsAs (res0 ::: res1 ::: res2)
    ext should contain theSameElementsAs (ext0 ::: ext1 ::: ext2)
  }

  "A singleton intersection" should "correspond to the conversion of the internal class" in {
    val cls0 = factory.getOWLClass(iriString0)
    val conj = factory.getOWLObjectIntersectionOf(cls0)
    val (res0, ext0) =
      convert(cls0, term0, List(), SkolemStrategy.None, Empty)
    val (res, ext) =
      convert(conj, term0, List(), SkolemStrategy.None, Empty)
    res should contain theSameElementsAs res0
    ext should contain theSameElementsAs ext0
  }

  "An object property" should "be converted into an atom with matching predicate" in {
    val prop = factory.getOWLObjectProperty(iriString0)
    for (sx <- suffixes) {
      val atom =
        TupleTableAtom.rdf(term0, IRI.create(iriString0 :: sx), term1)
      convert(prop, term0, term1, sx) shouldEqual atom
    }
  }

  "The inverse of an object property" should "be converted into an atom with matching negated predicate" in {
    val prop = factory.getOWLObjectProperty(iriString0)
    val inv = factory.getOWLObjectInverseOf(prop)
    for (sx <- Seq(Empty, Forward, Backward)) {
      val atom =
        TupleTableAtom.rdf(term0, IRI.create(iriString0 :: sx + Inverse), term1)
      convert(inv, term0, term1, sx) shouldEqual atom
    }
  }

  "A data property" should "be converted into an atom with matching predicate" in {
    val prop = factory.getOWLDataProperty(iriString0)
    for (suffix <- suffixes) {
      val atom =
        TupleTableAtom.rdf(term0, IRI.create(iriString0 :: suffix), term1)
      convert(prop, term0, term1, suffix) shouldEqual atom
    }
  }

}
