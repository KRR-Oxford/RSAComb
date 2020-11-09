package rsacomb

/* Java imports */
import java.util.HashMap
import java.util.stream.{Collectors, Stream}

import org.semanticweb.owlapi.model.{OWLOntology, OWLAxiom}
import org.semanticweb.owlapi.model.{
  OWLClass,
  OWLObjectProperty,
  OWLSubObjectPropertyOfAxiom,
  OWLObjectPropertyExpression,
  OWLObjectSomeValuesFrom,
  OWLSubClassOfAxiom
}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory
import org.semanticweb.owlapi.model.{IRI => OWLIRI}
import uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl

import tech.oxfordsemantic.jrdfox.client.{UpdateType, DataStoreConnection}
import tech.oxfordsemantic.jrdfox.logic.datalog.{Rule, TupleTableAtom, Negation}
import tech.oxfordsemantic.jrdfox.logic.expression.{
  Term,
  Variable,
  IRI,
  Resource
}
import tech.oxfordsemantic.jrdfox.logic.sparql.statement.SelectQuery

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

    // Gather TBox+RBox from original ontology
    lazy val tbox: List[OWLAxiom] =
      ontology
        .tboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toList())
        .asScala
        .toList

    lazy val rbox: List[OWLAxiom] =
      ontology
        .rboxAxioms(Imports.INCLUDED)
        .collect(Collectors.toList())
        .asScala
        .toList

    lazy val axioms: List[OWLAxiom] = tbox ++ rbox

    /* Retrieve individuals in the original ontology
     */
    lazy val individuals: List[IRI] =
      ontology
        .getIndividualsInSignature()
        .asScala
        .map(_.getIRI)
        .map(RDFoxUtil.owlapi2rdfox)
        .toList

    lazy val roles: List[OWLObjectPropertyExpression] =
      axioms
        .flatMap(_.objectPropertyExpressionsInSignature)
        .distinct

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

      val unsafe = this.unsafeRoles

      /* DEBUG: print rules in DL syntax and unsafe roles */
      //val renderer = new DLSyntaxObjectRenderer()
      //println("\nDL rules:")
      //axioms.foreach(x => println(renderer.render(x)))
      //println("\nUnsafe roles:")
      //println(unsafe)

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
      //println("\nDatalog roles:")
      //datalog.foreach(println)

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
      //println(graph)

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

    def filteringProgram(query: SelectQuery): FilteringProgram =
      FilteringProgram(query, individuals)

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

    // def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
    //   // Assuming just one role in the signature of a T5 axiom
    //   val roleR = axiom.objectPropertyExpressionsInSignature(0)
    //   val conflR = this.confl(roleR)
    //   // We just need the TBox to find
    //   val tbox = ontology
    //     .tboxAxioms(Imports.INCLUDED)
    //     .collect(Collectors.toSet())
    //     .asScala
    //   for {
    //     axiom1 <- tbox
    //     // TODO: is this an optimization or an error?
    //     if axiom1.isT5
    //     // We expect only one role coming out of a T5 axiom
    //     roleS <- axiom1.objectPropertyExpressionsInSignature
    //     // Triples ordering is among triples involving safe roles.
    //     if !unsafeRoles.contains(roleS)
    //     if conflR.contains(roleS)
    //     individual =
    //       if (axiom.hashCode < axiom1.hashCode) {
    //         RSA.internal("v0_" ++ axiom1.hashCode.toString())
    //       } else {
    //         RSA.internal("v1_" ++ axiom1.hashCode.toString())
    //       }
    //   } yield individual
    // }

    def cycle(axiom: OWLSubClassOfAxiom): Set[Term] = {
      val classes =
        axiom.classesInSignature.collect(Collectors.toList()).asScala
      val classA = classes(0)
      val roleR = axiom
        .objectPropertyExpressionsInSignature(0)
        .asInstanceOf[OWLObjectProperty]
      val classB = classes(1)
      cycle_aux(classA, roleR, classB)
    }

    def cycle_aux(
        classA: OWLClass,
        roleR: OWLObjectProperty,
        classB: OWLClass
    ): Set[Term] = {
      val conflR = this.confl(roleR)
      val classes = ontology
        .classesInSignature(Imports.INCLUDED)
        .collect(Collectors.toSet())
        .asScala
      for {
        classD <- classes
        roleS <- conflR
        classC <- classes
        // Keeping this check for now
        if !unsafeRoles.contains(roleS)
        tripleARB = Seq(classA, roleR, classB).hashCode
        tripleDSC = Seq(classD, roleS, classC).hashCode
        individual =
          if (tripleARB > tripleDSC) {
            RSA.internal("v1_" ++ tripleDSC.hashCode.toString())
          } else {
            // Note that this is also the case for
            // `tripleARB == tripleDSC`
            RSA.internal("v0_" ++ tripleDSC.hashCode.toString())
          }
      } yield individual
    }

    def unfold(axiom: OWLSubClassOfAxiom): Set[Term] =
      this.self(axiom) | this.cycle(axiom)

    object canonicalModel {

      import RDFoxUtil._

      val NIs: List[Rule] =
        individuals.map(a =>
          Rule.create(TupleTableAtom.rdf(a, IRI.RDF_TYPE, RSA.internal("NI")))
        )

      val rolesAdditionalRules: List[Rule] = {
        // Given a role (predicate) compute additional logic rules
        def additional(pred: String): Seq[Rule] = {
          val varX = Variable.create("X")
          val varY = Variable.create("Y")
          List(
            Rule.create(
              TupleTableAtom.rdf(varX, IRI.create(pred), varY),
              TupleTableAtom
                .rdf(
                  varX,
                  IRI.create(pred ++ RSASuffix.Forward.getSuffix),
                  varY
                )
            ),
            Rule.create(
              TupleTableAtom.rdf(varX, IRI.create(pred), varY),
              TupleTableAtom
                .rdf(
                  varX,
                  IRI.create(pred ++ RSASuffix.Backward.getSuffix),
                  varY
                )
            ),
            Rule.create(
              TupleTableAtom.rdf(
                varY,
                IRI.create(pred ++ RSASuffix.Backward.getSuffix ++ "_inv"),
                varX
              ),
              TupleTableAtom
                .rdf(
                  varX,
                  IRI.create(pred ++ RSASuffix.Forward.getSuffix),
                  varY
                )
            ),
            Rule.create(
              TupleTableAtom.rdf(
                varY,
                IRI.create(pred ++ RSASuffix.Forward.getSuffix ++ "_inv"),
                varX
              ),
              TupleTableAtom.rdf(
                varX,
                IRI.create(pred ++ RSASuffix.Backward.getSuffix),
                varY
              )
            )
          )
        }
        // Compute additional rules per role
        axioms
          .flatMap(
            _.objectPropertiesInSignature.collect(Collectors.toSet()).asScala
          )
          .distinct
          .map(_.getIRI.getIRIString)
          .flatMap(additional)
      }

      val rules: List[Rule] = {
        // Compute rules from ontology axioms
        val rules = axioms.flatMap(_.accept(this.ProgramGenerator))
        // Return full set of rules
        rules ++ rolesAdditionalRules ++ NIs
      }

      object ProgramGenerator
          extends RDFoxAxiomConverter(
            Variable.create("X"),
            unsafeRoles,
            SkolemStrategy.None,
            RSASuffix.None
          ) {

        private def rules1(axiom: OWLSubClassOfAxiom): List[Rule] = {
          val unfold = ontology.unfold(axiom).toList
          // Fresh Variables
          val v0 = RSA.internal("v0_" ++ axiom.hashCode.toString)
          val varX = Variable.create("X")
          // Predicates
          val atomA: TupleTableAtom = {
            val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
            TupleTableAtom.rdf(varX, IRI.RDF_TYPE, cls)
          }
          def in(t: Term): TupleTableAtom = {
            TupleTableAtom.rdf(
              t,
              RSA.internal("IN"),
              RSA.internal(unfold.hashCode.toString)
            )
          }
          def notIn(t: Term): Negation = Negation.create(in(t))
          val roleRf: TupleTableAtom = {
            val visitor =
              new RDFoxPropertyExprConverter(varX, v0, RSASuffix.Forward)
            axiom.getSuperClass
              .asInstanceOf[OWLObjectSomeValuesFrom]
              .getProperty
              .accept(visitor)
              .head
          }
          val atomB: TupleTableAtom = {
            val cls = axiom.getSuperClass
              .asInstanceOf[OWLObjectSomeValuesFrom]
              .getFiller
              .asInstanceOf[OWLClass]
              .getIRI
            TupleTableAtom.rdf(v0, IRI.RDF_TYPE, cls)
          }
          // TODO: To be consistent with the specifics of the visitor we are
          // returning facts as `Rule`s with true body. While this is correct
          // there is an easier way to import facts into RDFox. Are we able to
          // do that?
          val facts = unfold.map(x => Rule.create(in(x)))
          val rules = List(
            Rule.create(roleRf, atomA, notIn(varX)),
            Rule.create(atomB, atomA, notIn(varX))
          )
          facts ++ rules
        }

        private def rules2(axiom: OWLSubClassOfAxiom): List[Rule] = {
          val roleR =
            axiom.getSuperClass
              .asInstanceOf[OWLObjectSomeValuesFrom]
              .getProperty
          if (ontology.confl(roleR) contains roleR) {
            // Fresh Variables
            val v0 = RSA.internal("v0_" ++ axiom.hashCode.toString)
            val v1 = RSA.internal("v1_" ++ axiom.hashCode.toString)
            val v2 = RSA.internal("v2_" ++ axiom.hashCode.toString)
            // Predicates
            def atomA(t: Term): TupleTableAtom = {
              val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
              TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
            }
            def roleRf(t1: Term, t2: Term): TupleTableAtom = {
              val visitor =
                new RDFoxPropertyExprConverter(t1, t2, RSASuffix.Forward)
              roleR.accept(visitor).head
            }
            def atomB(t: Term): TupleTableAtom = {
              val cls = axiom.getSuperClass
                .asInstanceOf[OWLObjectSomeValuesFrom]
                .getFiller
                .asInstanceOf[OWLClass]
                .getIRI
              TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
            }
            //Rules
            List(
              Rule.create(roleRf(v0, v1), atomA(v0)),
              Rule.create(atomB(v1), atomA(v0)),
              Rule.create(roleRf(v1, v2), atomA(v1)),
              Rule.create(atomB(v2), atomA(v1))
            )
          } else {
            List()
          }
        }

        private def rules3(axiom: OWLSubClassOfAxiom): List[Rule] = {
          val cycle = ontology.cycle(axiom).toList
          val roleR =
            axiom.getSuperClass
              .asInstanceOf[OWLObjectSomeValuesFrom]
              .getProperty
          // Fresh Variables
          val v1 = RSA.internal("v1_" ++ axiom.hashCode.toString)
          // Predicates
          def atomA(t: Term): TupleTableAtom = {
            val cls = axiom.getSubClass.asInstanceOf[OWLClass].getIRI
            TupleTableAtom.rdf(t, IRI.RDF_TYPE, cls)
          }
          def roleRf(t: Term): TupleTableAtom = {
            val visitor =
              new RDFoxPropertyExprConverter(t, v1, RSASuffix.Forward)
            roleR.accept(visitor).head
          }
          val atomB: TupleTableAtom = {
            val cls = axiom.getSuperClass
              .asInstanceOf[OWLObjectSomeValuesFrom]
              .getFiller
              .asInstanceOf[OWLClass]
              .getIRI
            TupleTableAtom.rdf(v1, IRI.RDF_TYPE, cls)
          }
          cycle.flatMap { x =>
            List(
              Rule.create(roleRf(x), atomA(x)),
              Rule.create(atomB, atomA(x))
            )
          }
        }

        override def visit(axiom: OWLSubClassOfAxiom): List[Rule] = {
          if (axiom.isT5) {
            // TODO: get role in T5 axiom
            // Assuming one role here
            val role = axiom.objectPropertyExpressionsInSignature(0)
            if (ontology.unsafeRoles.contains(role)) {
              val visitor =
                new RDFoxAxiomConverter(
                  Variable.create("X"),
                  ontology.unsafeRoles,
                  SkolemStrategy.Standard(axiom.toString),
                  RSASuffix.Forward
                )
              axiom.accept(visitor)
            } else {
              rules1(axiom) ++ rules2(axiom) ++ rules3(axiom)
            }
          } else {
            // Fallback to standard OWL to LP translation
            super.visit(axiom)
          }
        }

        override def visit(axiom: OWLSubObjectPropertyOfAxiom): List[Rule] = {
          val varX = Variable.create("X")
          val varY = Variable.create("Y")
          val visitorF = new RDFoxAxiomConverter(
            Variable.create("X"),
            ontology.unsafeRoles,
            SkolemStrategy.None,
            RSASuffix.Forward
          )
          val visitorB = new RDFoxAxiomConverter(
            Variable.create("X"),
            ontology.unsafeRoles,
            SkolemStrategy.None,
            RSASuffix.Backward
          )
          axiom.accept(visitorB) ++ axiom.accept(visitorF)
        }

      }

    }
  } // implicit class RSAOntology

} // trait RSAOntology
