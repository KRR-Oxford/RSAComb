package rsacomb

/* Java imports */
// import java.io.File
import java.util.stream.{Collectors,Stream}

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
// import org.semanticweb.owlapi.model.OWLAxiomVisitorEx
import org.semanticweb.owlapi.model.parameters.Imports
// import org.semanticweb.owlapi.reasoner.OWLReasoner
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import tech.oxfordsemantic.jrdfox.logic.Variable

/* Scala imports */
import scala.collection.JavaConverters._

/* Local imports */
// import rsacomb.RSAAxiom 

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer

// import java.util.HashMap
// import java.util.stream.{Stream,Collectors}

// import org.semanticweb.owlapi.model.{AxiomType, ClassExpressionType, OWLObjectSomeValuesFrom}
// import org.semanticweb.owlapi.model.{OWLSubClassOfAxiom, OWLEquivalentClassesAxiom}
// import org.semanticweb.owlapi.model.OWLClassExpression
// import org.semanticweb.owlapi.model.IRI
// import org.semanticweb.owlapi.reasoner.{OWLReasonerFactory, OWLReasoner}
// import uk.ac.manchester.cs.owl.owlapi.OWLObjectPropertyImpl

// import tech.oxfordsemantic.jrdfox.Prefixes
// import tech.oxfordsemantic.jrdfox.client.{ConnectionFactory, ServerConnection, DataStoreConnection}
// import tech.oxfordsemantic.jrdfox.client.UpdateType
// import tech.oxfordsemantic.jrdfox.logic.{Rule, Atom, Literal, Term, Variable}
// import tech.oxfordsemantic.jrdfox.logic.{BuiltinFunctionCall, TupleTableName}
// import tech.oxfordsemantic.jrdfox.logic.{LogicFormat}
// import rsacomb.SkolemStrategy

trait RSAOntology {

  implicit class RSAOntology(ontology: OWLOntology) extends RSAAxiom {

    def isRSA: Boolean = {

      /* TODO: Steps for RSA check
       * 1) convert ontology axioms into LP rules
       * 2) call RDFox on the onto and compute materialization
       * 3) build graph from E(x,y) facts
       * 4) check if the graph is tree-like
       *    ideally this annotates the graph with info about the reasons
       *    why the ontology might not be RSA. This could help a second
       *    step of approximation of an Horn-ALCHOIQ to RSA
       */

      val tbox = 
        Stream.concat(ontology.tboxAxioms(Imports.INCLUDED), ontology.rboxAxioms(Imports.INCLUDED))
              .collect(Collectors.toList()).asScala
      val unsafe = ontology.getUnsafeRoles

      /* DEBUG: print rules in DL syntax */
      val renderer = new DLSyntaxObjectRenderer()
      println("\nDL rules:")
      tbox.foreach(x => println(renderer.render(x)))

      /* Ontology convertion into LP rules */
      println("\nLP rules:")
      for {
        axiom <- tbox
        visitor = new RDFoxAxiomConverter(Variable.create("x"), SkolemStrategy.ConstantRSA(axiom.toString), unsafe)
        rule  <- axiom.accept(visitor)
      } yield println(rule)

      /* DEBUG */
      true
    }

    def getUnsafeRoles: List[OWLObjectPropertyExpression] = {

      val factory = new StructuralReasonerFactory()
      val reasoner = factory.createReasoner(ontology)
      val tbox = ontology.tboxAxioms(Imports.INCLUDED).collect(Collectors.toList()).asScala
      val rbox = ontology.rboxAxioms(Imports.INCLUDED).collect(Collectors.toList()).asScala

      /* DEBUG: print rules in DL syntax */
      val renderer = new DLSyntaxObjectRenderer()
      println("\nT5 axioms:")
      for {
        axiom <- tbox
        if axiom.isT5
      } yield println(renderer.render(axiom))

            // def isT3(axiom : OWLAxiom) : Boolean = {
            // println(axiom)
            // axiom.getAxiomType match {
            //     case AxiomType.SUBCLASS_OF => 
            //     val axiom1 = axiom.asInstanceOf[OWLSubClassOfAxiom]
            //     axiom1.getSubClass().getClassExpressionType() == ClassExpressionType.OBJECT_SOME_VALUES_FROM &&
            //         axiom1.getSuperClass().getClassExpressionType() == ClassExpressionType.OWL_CLASS
            //     case AxiomType.EQUIVALENT_CLASSES => false // TODO
            // }
            // }

            // def isT4(axiom : OWLAxiom) : Boolean = {
            // axiom.getAxiomType match {
            //     case AxiomType.SUBCLASS_OF => 
            //     val axiom1 = axiom.asInstanceOf[OWLSubClassOfAxiom]
            //     axiom1.getSubClass().getClassExpressionType() == ClassExpressionType.OWL_CLASS &&
            //         axiom1.getSuperClass().getClassExpressionType() == ClassExpressionType.OBJECT_MAX_CARDINALITY
            //     case AxiomType.EQUIVALENT_CLASSES => false // TODO
            // }
            // }

            // def isT5(axiom : OWLAxiom) : Boolean = {
            // axiom.getAxiomType match {
            //     case AxiomType.SUBCLASS_OF => {
            //     val axiom1 = axiom.asInstanceOf[OWLSubClassOfAxiom]
            //     axiom1.getSubClass().getClassExpressionType() == ClassExpressionType.OWL_CLASS &&
            //         axiom1.getSuperClass().getClassExpressionType() == ClassExpressionType.OBJECT_SOME_VALUES_FROM
            //     }
            //     case AxiomType.EQUIVALENT_CLASSES => false // TODO
            // }
            // } 


            // println("T3")
            // for {
            // axiom <- ontology.tboxAxioms(Imports.INCLUDED)
            // //role <- axiom.objectPropertiesInSignature()
            // } yield println(axiom)

            // println("T4")
            // for {
            // axiom <- ontology.tboxAxioms(Imports.INCLUDED).filter(isT4)
            // //role <- axiom.objectPropertiesInSignature()
            // } yield println(axiom)


      /* DEBUG */
      List()
    }

  } // implicit class RSAOntology

} // trait RSAOntology