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

import scala.collection.mutable.Map
import scala.collection.JavaConverters._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLOntology, OWLAxiom, OWLLogicalAxiom}
import org.semanticweb.owlapi.model.{OWLObjectPropertyExpression}
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import tech.oxfordsemantic.jrdfox.logic.expression.Resource

object Ontology {

  /** Type wrapper representing a dependency graph for the ontology.
    *
    * The graph is returned along with a map associating each node (IRI
    * string of the resource), with the corresponding axiom in the
    * original TBox.
    */
  type DependencyGraph = (Graph[Resource, DiEdge], Map[String, OWLAxiom])

  /** Manager instance to interface with OWLAPI
    *
    * TODO: turn this into an implicit class parameter.
    */
  val manager = OWLManager.createOWLOntologyManager()
  //val factory = manager.getOWLDataFactory()

  /** Compute the RSA dependency graph for a set of axioms
    *
    * @param axioms set of input axioms (TBox) to build the dependency
    * graph.
    * @param datafiles data (ABox) to build the dependency graph.
    * @param unsafe list of unsafe roles in the TBox.
    *
    * @return a tuple containing the dependency graph and a map between
    * the newly introduced constants and the corresponding input axioms.
    *
    * @note no check on the ontology language is performed since the
    * construction of the dependency graph is computed regardless. The
    * input axioms are assumed to be normalized.
    */
  def dependencyGraph(
      axioms: List[OWLLogicalAxiom],
      datafiles: List[File],
      unsafe: List[OWLObjectPropertyExpression]
  ): DependencyGraph = {

    import org.semanticweb.owlapi.model.{
      OWLClassExpression,
      OWLObjectSomeValuesFrom,
      OWLDataSomeValuesFrom
    }
    import tech.oxfordsemantic.jrdfox.logic.datalog.Rule
    import tech.oxfordsemantic.jrdfox.logic.expression.{Term, Variable}
    import uk.ac.ox.cs.rsacomb.suffix._
    import uk.ac.ox.cs.rsacomb.converter._
    import uk.ac.ox.cs.rsacomb.util.{RDFoxUtil, RSA}
    import uk.ac.ox.cs.rsacomb.RSAUtil

    var nodemap = Map.empty[String, OWLAxiom]

    object RSAConverter extends RDFoxConverter {

      override def convert(
          expr: OWLClassExpression,
          term: Term,
          unsafe: List[OWLObjectPropertyExpression],
          skolem: SkolemStrategy,
          suffix: RSASuffix
      ): Shards =
        (expr, skolem) match {

          case (e: OWLObjectSomeValuesFrom, c: Constant) => {
            nodemap.update(c.iri.getIRI, c.axiom)
            val (res, ext) = super.convert(e, term, unsafe, skolem, suffix)
            if (unsafe contains e.getProperty)
              (RSA.PE(term, c.iri) :: RSA.U(c.iri) :: res, ext)
            else
              (RSA.PE(term, c.iri) :: res, ext)
          }

          case (e: OWLDataSomeValuesFrom, c: Constant) => {
            nodemap.update(c.iri.getIRI, c.axiom)
            val (res, ext) = super.convert(e, term, unsafe, skolem, suffix)
            if (unsafe contains e.getProperty)
              (RSA.PE(term, c.iri) :: RSA.U(c.iri) :: res, ext)
            else
              (RSA.PE(term, c.iri) :: res, ext)
          }

          case _ => super.convert(expr, term, unsafe, skolem, suffix)
        }
    }

    /* Ontology convertion into LP rules */
    val term = RSAUtil.genFreshVariable()
    val result = axioms.map(a =>
      RSAConverter.convert(a, term, unsafe, new Constant(a), Empty)
    )

    val datalog = result.unzip
    val facts = datalog._1.flatten
    var rules = datalog._2.flatten

    /* Open connection with RDFox */
    val (server, data) = RDFoxUtil.openConnection("rsa_dependency_graph")

    /* Add additional built-in rules */
    val varX = Variable.create("X")
    val varY = Variable.create("Y")
    rules = Rule.create(
      RSA.E(varX, varY),
      RSA.PE(varX, varY),
      RSA.U(varX),
      RSA.U(varY)
    ) :: rules
    /* Load facts and rules from ontology */
    RDFoxUtil.addFacts(data, facts)
    RDFoxUtil.addRules(data, rules)
    /* Load data files */
    RDFoxUtil.addData(data, datafiles: _*)

    /* Build the graph */
    val query = "SELECT ?X ?Y WHERE { ?X rsa:E ?Y }"
    val answers = RDFoxUtil.submitQuery(data, query, RSA.Prefixes).get
    var edges: Seq[DiEdge[Resource]] =
      answers.collect { case (_, Seq(n1, n2)) => n1 ~> n2 }
    val graph = Graph(edges: _*)

    /* Close connection to RDFox */
    RDFoxUtil.closeConnection(server, data)

    (graph, nodemap)
  }
}

/** A wrapper for a generic OWL2 ontology
  */
class Ontology(val axioms: List[OWLLogicalAxiom], val datafiles: List[File]) {

  /** Extend OWLAxiom functionalities */
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._

  /** OWLOntology based on input axioms
    *
    * This is mainly used to instantiate a new reasoner to be used in
    * the computation of unsafe roles.
    */
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

  lazy val dependencyGraph: Ontology.DependencyGraph =
    Ontology.dependencyGraph(axioms, datafiles, this.unsafe)

}
