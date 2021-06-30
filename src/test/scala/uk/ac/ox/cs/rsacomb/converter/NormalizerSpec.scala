/*
 * Copyright 2020, 2021 KRR Oxford
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.ac.ox.cs.rsacomb.converter

import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.OWLOntologyManager

import tech.oxfordsemantic.jrdfox.logic.datalog.TupleTableAtom
import tech.oxfordsemantic.jrdfox.logic.expression.{Variable, IRI}
import uk.ac.ox.cs.rsacomb.converter.RDFoxConverter
import uk.ac.ox.cs.rsacomb.suffix.{Empty, Forward, Backward, Inverse}
import uk.ac.ox.cs.rsacomb.converter.{Normalizer, SkolemStrategy, NoSkolem}

object NormalizerSpec {
  val manager = OWLManager.createOWLOntologyManager()
  val factory = manager.getOWLDataFactory
  val normalizer = new Normalizer()
}

class NormalizerSpec extends AnyFlatSpec with Matchers with LoneElement {

  import NormalizerSpec._

  "Equivalent classes" should "be split in pairwise subclass axioms" in {
    val cls1 = factory.getOWLClass("_:cls1")
    val cls2 = factory.getOWLClass("_:cls2")
    val cls3 = factory.getOWLClass("_:cls3")
    val equivalentClasses =
      factory.getOWLEquivalentClassesAxiom(cls1, cls2, cls3)
    normalizer.normalize(equivalentClasses) should contain theSameElementsAs
      Seq(
        factory.getOWLSubClassOfAxiom(cls1, cls2),
        factory.getOWLSubClassOfAxiom(cls1, cls3),
        factory.getOWLSubClassOfAxiom(cls2, cls1),
        factory.getOWLSubClassOfAxiom(cls2, cls3),
        factory.getOWLSubClassOfAxiom(cls3, cls1),
        factory.getOWLSubClassOfAxiom(cls3, cls2)
      ).flatMap(normalizer.normalize)
  }

  "Equivalent data properties" should "be split in pairwise subclass axioms" in {
    val prop1 = factory.getOWLDataProperty("_:prop1")
    val prop2 = factory.getOWLDataProperty("_:prop2")
    val prop3 = factory.getOWLDataProperty("_:prop3")
    val equivalentProps =
      factory.getOWLEquivalentDataPropertiesAxiom(prop1, prop2, prop3)
    normalizer.normalize(equivalentProps) should contain theSameElementsAs
      Seq(
        factory.getOWLSubDataPropertyOfAxiom(prop1, prop2),
        factory.getOWLSubDataPropertyOfAxiom(prop1, prop3),
        factory.getOWLSubDataPropertyOfAxiom(prop2, prop1),
        factory.getOWLSubDataPropertyOfAxiom(prop2, prop3),
        factory.getOWLSubDataPropertyOfAxiom(prop3, prop1),
        factory.getOWLSubDataPropertyOfAxiom(prop3, prop2)
      ).flatMap(normalizer.normalize)
  }

  "Equivalent object properties" should "be split in pairwise subclass axioms" in {
    val prop1 = factory.getOWLObjectProperty("_:prop1")
    val prop2 = factory.getOWLObjectProperty("_:prop2")
    val prop3 = factory.getOWLObjectProperty("_:prop3")
    val equivalentProps =
      factory.getOWLEquivalentObjectPropertiesAxiom(prop1, prop2, prop3)
    normalizer.normalize(equivalentProps) should contain theSameElementsAs
      Seq(
        factory.getOWLSubObjectPropertyOfAxiom(prop1, prop2),
        factory.getOWLSubObjectPropertyOfAxiom(prop1, prop3),
        factory.getOWLSubObjectPropertyOfAxiom(prop2, prop1),
        factory.getOWLSubObjectPropertyOfAxiom(prop2, prop3),
        factory.getOWLSubObjectPropertyOfAxiom(prop3, prop1),
        factory.getOWLSubObjectPropertyOfAxiom(prop3, prop2)
      ).flatMap(normalizer.normalize)
  }

  "Disjunction on the rhs" should "be shifted" in {
    def cls(n: Int) = factory.getOWLClass(s"_:class$n")
    val axiom1 =
      factory.getOWLSubClassOfAxiom(
        factory.getOWLObjectIntersectionOf(cls(1), cls(2), cls(3)),
        factory.getOWLObjectUnionOf(cls(4), cls(5))
      )
    val axiom2 =
      factory.getOWLSubClassOfAxiom(
        cls(1),
        factory.getOWLObjectUnionOf(cls(2), cls(3), cls(4))
      )
    val axiom3 =
      factory.getOWLSubClassOfAxiom(
        factory.getOWLObjectIntersectionOf(cls(1), cls(2), cls(3)),
        factory.getOWLObjectUnionOf(cls(4))
      )
    normalizer.normalize(axiom1) should have length 5
    normalizer.normalize(axiom2) should have length 5
    normalizer.normalize(axiom3) should have length 4
  }
  //"A class name" should "be converted into a single atom" in {
  //  val cls = factory.getOWLClass(iriString0)
  //  val atom = TupleTableAtom.rdf(term0, IRI.RDF_TYPE, IRI.create(iriString0))
  //  val (res, ext) =
  //    convert(cls, term0, List(), NoSkolem, Empty)
  //  res.loneElement shouldEqual atom
  //  ext shouldBe empty
  //}

  //"A intersection of classes" should "be converted into the union of the conversion of the classes" in {
  //  val cls0 = factory.getOWLClass(iriString0)
  //  val cls1 = factory.getOWLClass(iriString1)
  //  val cls2 = factory.getOWLClass(iriString2)
  //  val conj = factory.getOWLObjectIntersectionOf(cls0, cls1, cls2)
  //  val (res0, ext0) =
  //    convert(cls0, term0, List(), NoSkolem, Empty)
  //  val (res1, ext1) =
  //    convert(cls1, term0, List(), NoSkolem, Empty)
  //  val (res2, ext2) =
  //    convert(cls2, term0, List(), NoSkolem, Empty)
  //  val (res, ext) =
  //    convert(conj, term0, List(), NoSkolem, Empty)
  //  res should contain theSameElementsAs (res0 ::: res1 ::: res2)
  //  ext should contain theSameElementsAs (ext0 ::: ext1 ::: ext2)
  //}

  //"A singleton intersection" should "correspond to the conversion of the internal class" in {
  //  val cls0 = factory.getOWLClass(iriString0)
  //  val conj = factory.getOWLObjectIntersectionOf(cls0)
  //  val (res0, ext0) =
  //    convert(cls0, term0, List(), NoSkolem, Empty)
  //  val (res, ext) =
  //    convert(conj, term0, List(), NoSkolem, Empty)
  //  res should contain theSameElementsAs res0
  //  ext should contain theSameElementsAs ext0
  //}

  //"An object property" should "be converted into an atom with matching predicate" in {
  //  val prop = factory.getOWLObjectProperty(iriString0)
  //  for (sx <- suffixes) {
  //    val atom =
  //      TupleTableAtom.rdf(term0, IRI.create(iriString0 :: sx), term1)
  //    convert(prop, term0, term1, sx) shouldEqual atom
  //  }
  //}

  //"The inverse of an object property" should "be converted into an atom with inverted subject/object" in {
  //  val prop = factory.getOWLObjectProperty(iriString0)
  //  val inv = factory.getOWLObjectInverseOf(prop)
  //  for (sx <- Seq(Empty, Forward, Backward)) {
  //    val atom = TupleTableAtom.rdf(term1, IRI.create(iriString0 :: sx), term0)
  //    convert(inv, term0, term1, sx) shouldEqual atom
  //  }
  //}

  //"A data property" should "be converted into an atom with matching predicate" in {
  //  val prop = factory.getOWLDataProperty(iriString0)
  //  for (suffix <- suffixes) {
  //    val atom =
  //      TupleTableAtom.rdf(term0, IRI.create(iriString0 :: suffix), term1)
  //    convert(prop, term0, term1, suffix) shouldEqual atom
  //  }
  //}

}
