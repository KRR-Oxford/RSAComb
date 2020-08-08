package rsacomb

/* Java imports */
import java.util.stream.{Collectors,Stream}

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import tech.oxfordsemantic.jrdfox.logic.Variable

/* Scala imports */
import scala.collection.JavaConverters._

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer

/* Wrapper trait for the implicit class `RSAOntology`.
 */
trait RSAOntology {

  /* Implements additional features to reason about RSA ontologies
   * on top of `OWLOntology` from the OWLAPI.
   */
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
      // The reasoner is used to check unsafety condition for the ontology roles
      val factory = new StructuralReasonerFactory()
      val reasoner = factory.createReasoner(ontology)

      val tbox = ontology.tboxAxioms(Imports.INCLUDED).collect(Collectors.toSet()).asScala

      /* DEBUG: print rules in DL syntax */
      //val renderer = new DLSyntaxObjectRenderer()

      /* Checking for (1) unsafety condition:
       * 
       *    For all roles r1 appearing in an axiom of type T5, r1 is unsafe
       *    if there exists a role r2 (different from top) appearing in an axiom
       *    of type T3 and r1 is a subproperty of the inverse of r2.
       * 
       * TODO: We are not checking whether the class expression on the right in T3
       * is top. For now we can assume it is always the case.
       */
      val unsafe1 = for {
        axiom <- tbox
        if axiom.isT5
        role1 <- axiom.objectPropertyExpressionsInSignature
        roleSuper = role1 +: reasoner.superObjectProperties(role1).collect(Collectors.toList()).asScala
        roleSuperInv = roleSuper.map(_.getInverseProperty)
        axiom <- tbox
        if axiom.isT3
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
        roleSuper = role1 +: reasoner.superObjectProperties(role1).collect(Collectors.toList()).asScala
        roleSuperInv = roleSuper.map(_.getInverseProperty)
        axiom <- tbox
        if axiom.isT4
        role2 <- axiom.objectPropertyExpressionsInSignature 
        if roleSuper.contains(role2) || roleSuperInv.contains(role2)
      } yield role1

      /* TODO: We should be able to avoid this last conversion to List.
       * Maybe we should just move everything to Sets instead of Lists, since
       * they have a more straightforward conversion from Java collections.
       */
      (unsafe1 ++ unsafe2).toList
    }

  } // implicit class RSAOntology

} // trait RSAOntology