package rsacomb

/* Java imports */
import java.util.HashMap
import java.util.stream.{Collectors, Stream}

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.{
  OWLObjectProperty,
  OWLObjectPropertyExpression,
  OWLSubClassOfAxiom
}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.model.{IRI => OWLIRI}
import uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl

import tech.oxfordsemantic.jrdfox.client.{UpdateType, DataStoreConnection}
import tech.oxfordsemantic.jrdfox.logic.{Resource, Rule, Atom, Variable, IRI}

/* Scala imports */
import scala.collection.JavaConverters._
import scala.collection.mutable.Set
import scalax.collection.immutable.Graph
import scalax.collection.GraphEdge.UnDiEdge

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer
import tech.oxfordsemantic.jrdfox.logic._

/* Wrapper trait for the implicit class `RSAOntology`.
 */
trait RSAOntology {

  /* Implements additional features to reason about RSA ontologies
   * on top of `OWLOntology` from the OWLAPI.
   */
  implicit class RSAOntology(ontology: OWLOntology) extends RSAAxiom {

    /* Retrieve individuals in the original ontology
     */
    lazy val individuals: List[IRI] = {
      ontology
        .getIndividualsInSignature()
        .asScala
        .map(_.getIRI)
        .map(RDFoxUtil.owlapi2rdfox)
        .toList
    }

    // private val roles: Set[OWLObjectPropertyExpression] = {
    //   ontology
    //     .rboxAxioms(Imports.INCLUDED)
    //     .collect(Collectors.toSet())
    //     .asScala
    //     .flatMap(_.objectPropertyExpressionsInSignature)
    // }

    // OWLAPI reasoner for same easier tasks
    private val reasoner =
      (new StructuralReasonerFactory()).createReasoner(ontology)

    /* Steps for RSA check
     * 1) convert ontology axioms into LP rules
     * 2) call RDFox on the onto and compute materialization
     * 3) build graph from E(x,y) facts
     * 4) check if the graph is tree-like
     *    ideally this annotates the graph with info about the reasons
     *    why the ontology might not be RSA. This could help a second
     *    step of approximation of an Horn-ALCHOIQ to RSA
     */
    lazy val isRSA: Boolean = {

      val tbox = ontology.tboxAxioms(Imports.INCLUDED)
      val rbox = ontology.rboxAxioms(Imports.INCLUDED)
      val axioms =
        Stream
          .concat(tbox, rbox)
          .collect(Collectors.toList())
          .asScala
      val unsafe = this.unsafeRoles

      /* DEBUG: print rules in DL syntax and unsafe roles */
      val renderer = new DLSyntaxObjectRenderer()
      println("\nDL rules:")
      axioms.foreach(x => println(renderer.render(x)))
      println("\nUnsafe roles:")
      println(unsafe)

      /* Ontology convertion into LP rules */
      val datalog = for {
        axiom <- axioms
        visitor = new RDFoxAxiomConverter(
          RSA.getFreshVariable(),
          unsafe,
          SkolemStrategy.ConstantRSA(axiom.toString),
          RSASuffix.None
        )
        rule <- axiom.accept(visitor)
      } yield rule

      /* DEBUG: print datalog rules */
      println("\nDatalog roles:")
      datalog.foreach(println)

      // Open connection with RDFox
      val (server, data) = RDFoxUtil.openConnection("RSACheck")
      // Add Data (hardcoded for now)
      data.importData(UpdateType.ADDITION, RSA.Prefixes, ":a a :A .")

      /* Add built-in rules
       */
      data.importData(
        UpdateType.ADDITION,
        RSA.Prefixes,
        "<http://127.0.0.1/E>[?X,?Y] :- <http://127.0.0.1/PE>[?X,?Y], <http://127.0.0.1/U>[?X], <http://127.0.0.1/U>[?Y] ."
      )

      /* Add built-in rules
       */
      // data.importData(
      //   UpdateType.ADDITION,
      //   RSA.Prefixes,
      //   "[?entity, a, ?superClass] :- [?entity, a, ?class], [?class, rdfs:subClassOf, ?superClass] ."
      // )

      /* Add ontology rules
       */
      data.addRules(datalog.asJava)

      /* Build graph
       */
      val graph = this.rsaGraph(data);
      println(graph)

      // Close connection to RDFox
      RDFoxUtil.closeConnection(server, data)

      /* To check if the graph is tree-like we check for acyclicity in a
       * undirected graph.
       *
       * TODO: Implement additional checks (taking into account equality)
       */
      graph.isAcyclic
    }

    lazy val unsafeRoles: List[OWLObjectPropertyExpression] = {

      val tbox = ontology
        .tboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toSet())
        .asScala

      /* DEBUG: print rules in DL syntax */
      //val renderer = new DLSyntaxObjectRenderer()

      /* Checking for (1) unsafety condition:
       *
       *    For all roles r1 appearing in an axiom of type T5, r1 is unsafe
       *    if there exists a role r2 (different from top) appearing in an axiom
       *    of type T3 and r1 is a subproperty of the inverse of r2.
       */
      val unsafe1 = for {
        axiom <- tbox
        if axiom.isT5
        role1 <- axiom.objectPropertyExpressionsInSignature
        roleSuper =
          role1 +: reasoner
            .superObjectProperties(role1)
            .collect(Collectors.toList())
            .asScala
        roleSuperInv = roleSuper.map(_.getInverseProperty)
        axiom <- tbox
        if axiom.isT3 && !axiom.isT3top
        role2 <- axiom.objectPropertyExpressionsInSignature
        if roleSuperInv.contains(role2)
      } yield role1

      /* Checking for (2) unsafety condition:
       *
       *    For all roles p1 appearing in an axiom of type T5, p1 is unsafe if
       *    there exists a role p2 appearing in an axiom of type T4 and p1 is a
       *    subproperty of either p2 or the inverse of p2.
       *
       */
      val unsafe2 = for {
        axiom <- tbox
        if axiom.isT5
        role1 <- axiom.objectPropertyExpressionsInSignature
        roleSuper =
          role1 +: reasoner
            .superObjectProperties(role1)
            .collect(Collectors.toList())
            .asScala
        roleSuperInv = roleSuper.map(_.getInverseProperty)
        axiom <- tbox
        if axiom.isT4
        role2 <- axiom.objectPropertyExpressionsInSignature
        if roleSuper.contains(role2) || roleSuperInv.contains(role2)
      } yield role1

      (unsafe1 ++ unsafe2).toList
    }

    lazy val canonicalModel: List[Rule] = {
      // Compute program to generate canonical model
      val tbox =
        ontology
          .tboxAxioms(Imports.INCLUDED)
          .collect(Collectors.toList())
          .asScala
          .toList
      val rbox =
        ontology
          .rboxAxioms(Imports.INCLUDED)
          .collect(Collectors.toList())
          .asScala
          .toList
      val axioms = tbox ++ rbox
      val varX = Variable.create("X")
      val visitor = ProgramGenerator(ontology, varX)
      val facts = ProgramGenerator.NIs(individuals)
      val rules1 = ProgramGenerator.generateRoleRules(
        axioms
          .flatMap(
            _.objectPropertiesInSignature.collect(Collectors.toSet()).asScala
          )
          .toSet
      )
      val rules2 = axioms.flatMap(_.accept(visitor))

      rules1 ++ rules2
      // Call RDFox to generate the canonical model
    }

    private def rsaGraph(
        data: DataStoreConnection
    ): Graph[Resource, UnDiEdge] = {
      val query = "SELECT ?X ?Y WHERE { ?X internal:E ?Y }"
      val cursor =
        data.createCursor(RSA.Prefixes, query, new HashMap[String, String]());
      var mul = cursor.open()
      var edges: List[UnDiEdge[Resource]] = List()
      while (mul > 0) {
        edges = UnDiEdge(cursor.getResource(0), cursor.getResource(1)) :: edges
        mul = cursor.advance()
      }
      Graph(edges: _*)
    }

    def filteringProgram(query: Query): List[Rule] =
      FilteringProgram(query, individuals).rules

    // TODO: the following functions needs testing
    def confl(
        role: OWLObjectPropertyExpression
    ): Set[OWLObjectPropertyExpression] = {

      val invSuperRoles = reasoner
        .superObjectProperties(role)
        .collect(Collectors.toSet())
        .asScala
        .addOne(role)
        .map(_.getInverseProperty)

      invSuperRoles
        .flatMap(x =>
          reasoner
            .subObjectProperties(x)
            .collect(Collectors.toSet())
            .asScala
            .addOne(x)
        )
        .filterNot(_.isOWLBottomObjectProperty())
        .filterNot(_.getInverseProperty.isOWLTopObjectProperty())
    }

    def self(axiom: OWLSubClassOfAxiom): Set[Term] = {
      // Assuming just one role in the signature of a T5 axiom
      val role = axiom.objectPropertyExpressionsInSignature(0)
      if (this.confl(role).contains(role)) {
        Set(
          RSA.internal("v0_" ++ axiom.hashCode.toString()),
          RSA.internal("v1_" ++ axiom.hashCode.toString())
        )
      } else {
        Set()
      }
    }

    // TODO: this implementation is not correct when taking into
    // account equality.
    def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
      // Assuming just one role in the signature of a T5 axiom
      val roleR = axiom.objectPropertyExpressionsInSignature(0)
      val conflR = this.confl(roleR)
      // We just need the TBox to find
      val tbox = ontology
        .tboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toSet())
        .asScala
      for {
        axiom1 <- tbox
        // TODO: is this an optimization or an error?
        if axiom1.isT5
        // We expect only one role coming out of a T5 axiom
        roleS <- axiom1.objectPropertyExpressionsInSignature
        // Triples ordering is among triples involving safe roles.
        if !unsafeRoles.contains(roleS)
        if conflR.contains(roleS)
        individual =
          if (axiom.hashCode < axiom1.hashCode) {
            RSA.internal("v0_" ++ axiom1.hashCode.toString())
          } else {
            RSA.internal("v1_" ++ axiom1.hashCode.toString())
          }
      } yield individual
    }

    def unfold(axiom: OWLSubClassOfAxiom): Set[Term] =
      this.self(axiom) | this.cycle(axiom)

  } // implicit class RSAOntology

} // trait RSAOntology
