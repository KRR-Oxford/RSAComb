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

package uk.ac.ox.cs.rsacomb.ontology

import java.io.File

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLOntology, OWLAxiom, OWLLogicalAxiom}
import org.semanticweb.owlapi.model.{OWLObjectPropertyExpression}
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

object Ontology {

  /** Manager instance to interface with OWLAPI */
  val manager = OWLManager.createOWLOntologyManager()
  //val factory = manager.getOWLDataFactory()

}

/** A wrapper for
  */
class Ontology(val axioms: List[OWLLogicalAxiom], val datafiles: List[File]) {

  /** Extend OWLAxiom functionalities */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  /** OWLOntology based on input axioms */
  private val ontology: OWLOntology =
    Ontology.manager.createOntology((axioms: List[OWLAxiom]).asJava)

  /** OWLAPI internal reasoner for ontology */
  private val reasoner =
    (new StructuralReasonerFactory()).createReasoner(ontology)

  /** Unsafe roles in the ontology
    *
    * Unsafety conditions are the following:
    *
    * 1) For all roles r1 appearing in an axiom of type T5, r1 is unsafe
    *    if there exists a role r2 (different from top) appearing in an
    *    axiom of type T3 and r1 is a subproperty of the inverse of r2.
    *
    * 2) For all roles p1 appearing in an axiom of type T5, p1 is unsafe
    *    if there exists a role p2 appearing in an axiom of type T4 and
    *    p1 is a subproperty of either p2 or the inverse of p2.
    */
  lazy val unsafe: List[OWLObjectPropertyExpression] = {

    /* Checking for unsafety condition (1) */
    val unsafe1 = for {
      axiom <- axioms
      if axiom.isT5
      role1 <- axiom.objectPropertyExpressionsInSignature
      roleSuper = role1 +: reasoner.superObjectProperties(role1)
      axiom <- axioms
      if axiom.isT3 && !axiom.isT3top
      role2 <- axiom.objectPropertyExpressionsInSignature
      if roleSuper contains role2.getInverseProperty
    } yield role1

    /* Checking for unsafety condition (2) */
    val unsafe2 = for {
      axiom <- axioms
      if axiom.isT5
      role1 <- axiom.objectPropertyExpressionsInSignature
      roleSuper = role1 +: reasoner.superObjectProperties(role1)
      axiom <- axioms
      if axiom.isT4
      role2 <- axiom.objectPropertyExpressionsInSignature
      if roleSuper.contains(role2) ||
        roleSuper.contains(role2.getInverseProperty)
    } yield role1

    unsafe1 ++ unsafe2
  }
}
