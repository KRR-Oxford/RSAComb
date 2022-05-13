/*
 * Copyright 2020-2022 KRR Oxford
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

import java.util.stream.Collectors

import scala.collection.JavaConverters._
import scala.collection.mutable.Map
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  FilterAtom,
  Rule,
  TupleTableAtom,
  TupleTableName,
}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  FunctionCall,
  IRI,
  Literal,
  Resource,
  Term,
}

import uk.ac.ox.cs.rsacomb.approximation.Approximation
import uk.ac.ox.cs.rsacomb.converter._
import uk.ac.ox.cs.rsacomb.suffix._
import uk.ac.ox.cs.rsacomb.util.{DataFactory,RDFoxUtil,RSA}

object Ontology {

  /** Simplify conversion between Java and Scala collections */
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._
  
  /** Name of the RDFox data store used for the RSA check */
  val DataStore = "rsa_dependency_graph"
  /* RSA check named graph */
  val RSACheck: IRI = RDFoxUtil.getNamedGraph(DataStore, "RSACheck")
  /* Named graphs for some necessary RBox reasoning */
  private val RBoxProxy: IRI = RDFoxUtil.getNamedGraph(DataStore, "RBoxProxy")
  val RBoxReasoning: IRI = RDFoxUtil.getNamedGraph(DataStore, "RBoxReasoning")

  /** Manager instance to interface with OWLAPI
    *
    * TODO: turn this into an implicit class parameter.
    */
  val manager = OWLManager.createOWLOntologyManager()
  val factory = manager.getOWLDataFactory()

  def apply(ontology: OWLOntology, datafiles: List[os.Path]): Ontology = {

    /** TBox axioms */
    var tbox: List[OWLLogicalAxiom] =
      ontology
        .tboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toList())
        .collect { case a: OWLLogicalAxiom => a }

    /** RBox axioms */
    var rbox: List[OWLLogicalAxiom] =
      ontology
        .rboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toList())
        .collect { case a: OWLLogicalAxiom => a }

    /** ABox axioms
      *
      * @note this represents only the set of assertions contained in the
      * ontology file. Data files specified in `datafiles` are directly
      * imported in RDFox due to performance issues when trying to import
      * large data files via OWLAPI.
      */
    var abox: List[OWLLogicalAxiom] =
      ontology
        .aboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toList())
        .collect { case a: OWLLogicalAxiom => a }

    new Ontology(ontology, abox ::: tbox ::: rbox, datafiles)
  }

  def apply(ontofile: os.Path, datafiles: List[os.Path]): Ontology = {
    val ontology = manager.loadOntologyFromOntologyDocument(ontofile.toIO)
    Ontology(ontology, datafiles)
  }

}

/** A wrapper for a generic OWL2 ontology
  *
  * @param origin reference to the wrapped [[OWLOntology]]
  * @param axioms list of axioms (TBox + RBox) in the ontology
  * @param datafiles files containing ABox data
  */
class Ontology (
    val origin: OWLOntology,
    val axioms: List[OWLLogicalAxiom],
    val datafiles: List[os.Path]
) {

  import uk.ac.ox.cs.rsacomb.util.RDFoxDSL._

  import uk.ac.ox.cs.rsacomb.implicits.RDFox._
  import uk.ac.ox.cs.rsacomb.implicits.RSAAxiom._
  import uk.ac.ox.cs.rsacomb.implicits.JavaCollections._


  /** OWLOntology based on input axioms
    *
    * This is mainly used to instantiate a new reasoner to be used in
    * the computation of unsafe roles.
    */
  protected val ontology: OWLOntology =
    Ontology.manager.createOntology((axioms: List[OWLAxiom]).asJava)

  /** OWLAPI internal reasoner for the current ontology */
  protected val reasoner =
    (new StructuralReasonerFactory()).createReasoner(ontology)

  /** Returns individuals in ontology */
  val individuals: List[IRI] =
    ontology.getIndividualsInSignature(Imports.INCLUDED).map(_.getIRI)

  /** Returns literals in ontology */
  val literals: List[Literal] =
    axioms.collect { case a: OWLDataPropertyAssertionAxiom => a } .map(_.getObject)

  /** Returns concepts in ontology */
  val concepts: List[OWLClass] = ontology.getClassesInSignature()

  /** Retrieve (object) roles in ontology */
  val objroles: List[OWLObjectPropertyExpression] =
    axioms.flatMap(_.objectPropertyExpressionsInSignature).distinct

  /** Retrieve (data) roles in ontology */
  val dataroles: List[OWLDataProperty] = origin.getDataPropertiesInSignature

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

  /** Top axiomatization rules.
    *
    * For each concept/role *in the ontology file* introduce a rule to
    * derive `owl:Thing`.
    *
    * @note this might not be enough in cases where data files contain
    * concept/roles that are not in the ontology file. While this is
    * non-standard, it is not forbidden either and may cause problems
    * since not all individuals are considered part of `owl:Thing`.
    *
    * @note this is a naïve implementation of top axiomatization and
    * might change in the future. The ideal solution would be for RDFox
    * to take care of this, but at the time of writing this is not
    * compatible with the way we are using the tool.
    *
    * TODO: use RDFox DSL
    */
  protected def topAxioms(tt: IRI): List[Rule] = {
    val graph = TupleTableName.create(tt.getIRI)
    Rule.create(
      TupleTableAtom.create(graph, v"X", IRI.RDF_TYPE, IRI.THING),
      TupleTableAtom.create(graph, v"X", IRI.RDF_TYPE, v"Y")
    ) :: objroles.map(r => {
      val name = r match {
        case x: OWLObjectProperty => x.getIRI.getIRIString
        case x: OWLObjectInverseOf =>
          x.getInverse.getNamedProperty.getIRI.getIRIString :: Inverse
      }
      Rule.create(
        List(
          TupleTableAtom.create(graph, v"X", IRI.RDF_TYPE, IRI.THING),
          TupleTableAtom.create(graph, v"Y", IRI.RDF_TYPE, IRI.THING)
        ),
        List(TupleTableAtom.create(graph, v"X", name, v"Y"))
      )
    }) ::: dataroles.map(r => {
      val name = r.getIRI.getIRIString
      Rule.create(
        List(
          TupleTableAtom.create(graph, v"X", IRI.RDF_TYPE, IRI.THING),
          TupleTableAtom.create(graph, v"Y", IRI.RDF_TYPE, IRI.THING)
        ),
        List(TupleTableAtom.create(graph, v"X", name, v"Y"))
      )
    })
  }

  /** Equality axiomatization rules.
    *
    * Introduce reflexivity, simmetry and transitivity rules for a naïve
    * equality axiomatization.
    *
    * @note that we are using a custom `congruent` predicate to indicate
    * equality. This is to avoid interfering with the standard
    * `owl:sameAs`.
    *
    * @note RDFox is able to handle equality in a "smart" way, but this
    * behaviour is incompatible with other needed features like
    * negation-as-failure and aggregates.
    *
    * @todo naïve substitution rules might not be very efficient. We
    * should look into other ways of implementing this (e.g.,
    * singularization).
    */
  protected def equalityAxioms(tt: IRI): List[Rule] = {
    val graph = RDFoxGraph(tt)
    val g = TupleTableName.create(tt.getIRI)
    // Equality properties
    val properties = List(
      // Reflexivity
      graph(v"X", RSA.CONGRUENT, v"X") :- graph(v"X", IRI.RDF_TYPE, IRI.THING),
      // Simmetry
      graph(v"Y", RSA.CONGRUENT, v"X") :- graph(v"X", RSA.CONGRUENT, v"Y"),
      // Transitivity
      graph(v"X", RSA.CONGRUENT, v"Z") :-
        graph(v"X", RSA.CONGRUENT, v"Y") + graph(v"Y", RSA.CONGRUENT, v"Z"),
    )
    /* Equality substitution rules */
    val conceptSub =
      graph(v"Y", IRI.RDF_TYPE, v"Z") :-
        graph(v"X", RSA.CONGRUENT, v"Y") + graph(v"X", IRI.RDF_TYPE, v"Z") +
        FilterAtom.create(FunctionCall.notEqual(v"Z", RSA.NAMED))
    val roleSub = objroles.flatMap(r => {
        val name = r match {
          case x: OWLObjectProperty => x.getIRI.getIRIString
          case x: OWLObjectInverseOf =>
            x.getInverse.getNamedProperty.getIRI.getIRIString :: Inverse
        }
        List(
          graph(v"Z", name, v"Y") :-
            graph(v"X", RSA.CONGRUENT, v"Z") + graph(v"X", name, v"Y"),
          graph(v"Y", name, v"Z") :-
            graph(v"X", RSA.CONGRUENT, v"Z") + graph(v"Y", name, v"X")
        )
      })
    conceptSub :: properties ::: roleSub
  }

  /** Type wrapper representing a dependency graph for the ontology. */
  type DependencyGraph = Graph[Resource, DiEdge]

  /** Compute the RSA dependency graph for a set of axioms
    *
    * After calling this at least once, the materialisation M_{RSA} is
    * available in RDFox in the named graph [[Ontology.RSACheck]].
    * From this, the dependency graph can be derived by looking at the
    * instances of the predicate `E`.
    *
    * Furthermore, the [[Ontology.RBoxReasoning]] named graph is
    * populated with all relevant rules to reason over the RBox of the
    * ontology.
    *
    * @return a map between the newly introduced constants and the
    * corresponding input axioms.
    *
    * TODO: use RDFox DSL
    */
  lazy val dependencyGraph: (DependencyGraph, Map[String, OWLAxiom]) = {
    /* Keep track of the mapping between nodes and axioms */
    var nodemap = Map.empty[String, OWLAxiom]
    /* Computation of the dependency graph is confined to the
     * [[Ontology.RSACheck]] named graph.
     */
    val tt = TupleTableName.create(Ontology.RSACheck.getIRI)

    /** Custom converter to generate dependency graph.
      *
      * @note this corresponds to Definition 3 in the RSA paper.
      */
    object RSACheckConverter extends RDFoxConverter {
      import org.semanticweb.owlapi.model.{
        OWLClassExpression,
        OWLObjectSomeValuesFrom,
        OWLDataSomeValuesFrom
      }

      override val graph = tt
      override def convert(
          expr: OWLClassExpression,
          term: Term,
          unsafe: List[OWLObjectPropertyExpression],
          skolem: SkolemStrategy,
          suffix: RSASuffix
      )(implicit fresh: DataFactory): Shards =
        (expr, skolem) match {

          case (e: OWLObjectSomeValuesFrom, c: Constant) => {
            nodemap.update(c.iri.getIRI, c.axiom)
            val (res, ext) =
              super.convert(e, term, unsafe, skolem, suffix)(fresh)
            if (unsafe contains e.getProperty)
              (RSA.PE(tt)(term, c.iri) :: RSA.U(tt)(c.iri) :: res, ext)
            else
              (RSA.PE(tt)(term, c.iri) :: res, ext)
          }

          //case (e: OWLDataSomeValuesFrom, c: Constant) => {
          //  nodemap.update(c.iri.getIRI, c.axiom)
          //  val (res, ext) =
          //    super.convert(e, term, unsafe, skolem, suffix)(fresh)
          //  if (unsafe contains e.getProperty)
          //    (RSA.PE(tt)(term, c.iri) :: RSA.U(tt)(c.iri) :: res, ext)
          //  else
          //    (RSA.PE(tt)(term, c.iri) :: res, ext)
          //}

          case _ => super.convert(expr, term, unsafe, skolem, suffix)(fresh)
        }
    }

    /* Ontology convertion into LP rules */
    val result = axioms.map(a =>
      RSACheckConverter.convert(a, v"X", unsafe, new Constant(a), Empty)
    )
    val datalog = result.unzip
    val facts = datalog._1.flatten
    var rules = datalog._2.flatten

    /* Open connection with RDFox */
    val (server, data) = RDFoxUtil.openConnection(Ontology.DataStore)
    /* Upload data from data files */
    RDFoxUtil.addData(data, Ontology.RSACheck, datafiles: _*)
    /* Top/equality axiomatization */
    RDFoxUtil.updateData(data, 
      s"""
      INSERT { 
        GRAPH ${Ontology.RSACheck} { ?X a ${IRI.THING} }
      } WHERE {
        GRAPH ${Ontology.RSACheck} { ?X ?Y ?Z }
      }
      """
    )
    RDFoxUtil.updateData(data, 
      s"""
      INSERT { 
        GRAPH ${Ontology.RSACheck} { ?Z a ${IRI.THING} }
      } WHERE {
        GRAPH ${Ontology.RSACheck} { ?X ?Y ?Z }.
        FILTER( ?Y != a )
      }
      """
    )
    RDFoxUtil.addRules(data,
      topAxioms(Ontology.RSACheck) ++ equalityAxioms(Ontology.RSACheck)
    )
    /* Introduce `rsacomb:Named` concept */
    /* From data */
    RDFoxUtil.updateData(data, 
      s"""
      INSERT { 
        GRAPH ${Ontology.RSACheck} { ?X a ${RSA.NAMED} }
      } WHERE {
        GRAPH ${Ontology.RSACheck} { ?X a ${IRI.THING} }
      }
      """
    )
    /* From ontology */
    val named = individuals.map(RSA.Named(Ontology.RSACheck)(_))
    RDFoxUtil.addFacts(data, Ontology.RSACheck, named)
    /* Add rule to build dependency graph */
    rules = Rule.create(
      RSA.E(tt)(v"X", v"Y"),
      RSA.PE(tt)(v"X", v"Y"),
      RSA.U(tt)(v"X"),
      RSA.U(tt)(v"Y")
    ) :: rules
    /* Add logic program */
    RDFoxUtil.addFacts(data, Ontology.RSACheck, facts)
    RDFoxUtil.addRules(data, rules)

    /* Build the dependency graph */
    val query = s"SELECT ?X ?Y WHERE { graph ${Ontology.RSACheck} { ?X ${RSA("E")} ?Y }"
    val answers = RDFoxUtil.submitQuery(data, query, RSA.Prefixes).getOrElse(RDFoxUtil.QueryAnswers())
    var edges: Seq[DiEdge[Resource]] =
      answers.collect { case (_, Seq(n1, n2)) => n1 ~> n2 }
    val graph = Graph(edges: _*)

    /* Setup RBox reasoner */
    RDFoxUtil.addAxioms(data, Ontology.RBoxProxy, Ontology.RBoxReasoning, axioms)
    /* Add reflexive `subObjectProperty`s */
    val refls = objroles.map(r => Ontology.factory.getOWLSubObjectPropertyOfAxiom(r, r))
    RDFoxUtil.addAxioms(data, Ontology.RBoxProxy, Ontology.RBoxReasoning, refls)
    /* Add RBox reasoning rules */
    RDFoxUtil.addRules(data, RSA.RBoxReasoning(Ontology.RBoxReasoning))

    /* Close connection to RDFox */
    RDFoxUtil.closeConnection(server, data)

    (graph, nodemap)
  }

  /** Check whether the ontology is RSA
    *
    * The following checks are performed:
    * 1) The ontology is Horn-ALCHOIQ
    * 1) The dependency graph is an oriented forest
    * 2) The ontology is equality safe.
    * 
    * @note at the moment this assumes that the ontology is normalized.
    *
    * @see Def.3 in RSA paper for a formal definition of these concepts.
    */
  lazy val isRSA: Boolean = {
    this.isHornALCHOIQ && {
      val (graph,nodemap) = dependencyGraph
      val (server,data) = RDFoxUtil.openConnection(Ontology.DataStore)
      /* The dependecy graph is an oriented forest, i.e., is
       * an *acyclic* graph with max in-degree 1.
       */
      val check1 = graph.isAcyclic && graph.nodes.forall(_.inDegree <= 1)
      /* Equality safety: condition 1 */
      val check2 = RDFoxUtil.submitQuery(data, s"""
        ASK {
          graph ${Ontology.RSACheck} { ?w ${RSA.CONGRUENT} ?t } .
          filter ( ?w != ?t ) .
          graph ${Ontology.RSACheck} { ?t ?r [ a ${RSA.U} ] } .
          graph ${Ontology.RBoxReasoning} {
            ?r rdfs:subPropertyOf [ owl:inverseOf ?s ] .
            ?x rdf:type owl:Restriction ;
               owl:onProperty ?s ;
               owl:maxQualifiedCardinality "1"^^xsd:nonNegativeInteger ;
               owl:onClass ?b .
            ?a rdfs:subClassOf ?b .
          } .
        }
      """).get
      /* Equality safety: condition 2 */
      val check3 = RDFoxUtil.submitQuery(data, s"""
        ASK {
          graph ${Ontology.RSACheck} { 
            ?u ?s ?a ; a ${RSA.U} .
            ?a ?r ?u ; a ${RSA.NI} .
          } .
          graph ${Ontology.RBoxReasoning} {
            ?r rdfs:subPropertyOf ?r1 .
            ?r1 ${RSA("subPropertyOfTrans")} ?t .
            ?t owl:inverseOf ?ti .
            ?s rdfs:subPropertyOf ?s1 .
            ?s1 ${RSA("subPropertyOfTrans")} ?ti .
          }
        }
      """).get
      check1 && check2.isEmpty && check3.isEmpty
    }
  }

  /** Checks whether the ontology is Horn-ALCHOIQ.
    *
    * @note at the moment this assumes that the ontology is normalized.
    *
    * @see [[uk.ac.ox.cs.rsacomb.ontology.Ontology.normalize]]
    *
    * TODO: make the function independed of normalization, or
    * alternatively force normalization.
    */
  lazy val isHornALCHOIQ: Boolean =
    axioms.forall(axiom => 
      axiom match {
        case a: OWLSubClassOfAxiom => {
          val sub = a.getSubClass.getNNF
          val sup = a.getSuperClass.getNNF
          (sub, sup) match {
            case (sub: OWLObjectAllValuesFrom, _) => false
            case (sub: OWLDataAllValuesFrom, _)   => false
            case (_, sup: OWLDataAllValuesFrom)   => false
            case (sub: OWLObjectMinCardinality, _) if sub.getCardinality >= 2 =>
              false
            case (sub: OWLDataMinCardinality, _) if sub.getCardinality >= 2 =>
              false
            case (_, sup: OWLObjectMinCardinality) if sup.getCardinality >= 2 =>
              false
            case (_, sup: OWLDataMinCardinality) if sup.getCardinality >= 2 =>
              false
            case (sub: OWLObjectMaxCardinality, _) => false
            case (sub: OWLDataMaxCardinality, _)   => false
            case (_, sup: OWLObjectMaxCardinality) if sup.getCardinality >= 2 =>
              false
            case (_, sup: OWLDataMaxCardinality) if sup.getCardinality >= 1 =>
              false
            case (_, sup: OWLObjectOneOf) if sup.getIndividuals.length > 2 =>
              false
            case (sub: OWLObjectHasSelf, _) => false
            case (_, sup: OWLObjectHasSelf) => false
            case (_, sup: OWLObjectUnionOf) => false
            case _                          => true
          }
        }
        case a: OWLTransitiveObjectPropertyAxiom => false
        case a: OWLReflexiveObjectPropertyAxiom  => false
        case a: OWLSubPropertyChainOfAxiom       => false
        case a: OWLAsymmetricObjectPropertyAxiom => false
        case a                                   => true
      }
  )

  /** Normalize the ontology according to the given normalizer
    *
    * @param normalizer the normalization technique to be used.
    * @return a new normalized [[Ontology]].
    */
  def normalize(normalizer: Normalizer): Ontology =
    new Ontology(
      origin,
      axioms flatMap normalizer.normalize,
      datafiles
    )

  /** Approximate the ontology according to the given approximation
    * technique.
    *
    * @param approximation the approximation to be used on the ontology.
    * @return the result of the approximation.
    */
  def approximate[T](approximation: Approximation[T]): T =
    approximation approximate this
}
