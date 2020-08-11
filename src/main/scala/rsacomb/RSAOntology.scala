package rsacomb

/* Java imports */
import java.util.HashMap
import java.util.stream.{Collectors,Stream}

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.Variable
import tech.oxfordsemantic.jrdfox.client.UpdateType


/* Scala imports */
import scala.collection.JavaConverters._

/* Debug only */
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer
import java.io.OutputStream

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
      val datalog = for {
        axiom <- tbox
        visitor = new RDFoxAxiomConverter(Variable.create("x"), SkolemStrategy.ConstantRSA(axiom.toString), unsafe)
        rule  <- axiom.accept(visitor)
      } yield rule

      val prefixes = new Prefixes()
      prefixes.declarePrefix(":", "http://example.com/rsa_example.owl#")

      // Open connection with RDFox
      val (server,data) = RDFox.openConnection("RSACheck")
      // Add Data (hardcoded for now)
      data.importData(UpdateType.ADDITION, prefixes,":a a :A .")
      /* Add Datalog rules
       *
       * NOTE:
       * - using the `addRules(...)` method in `DataStoreConnection` is not working as expected, complaining
       *   about missing TupleTable entries;
       * - weirdly enough, the same error is returned when trying to pass the rules to the `importData` method,
       *   simply turning them into strings. It seems like the `toString` implementation of `Rule` uses parenthesis
       *   for predicate arguments (e.g., `<predicate>(?X,?Y)`) while the specification for the proprietary RDFox
       *   syntax uses squared brackets (e.g., `<preditate>[?X,?Y]`).
       */
      data.importData(
        UpdateType.ADDITION,
        prefixes,
        datalog.foldLeft("")((str,rule) => str ++ "\n" ++ rule.toString().replace("(", "[").replace(")","]"))
      )

      // Retrieve all instances of PE
      println("\nQuery results:")
      data.evaluateQuery(prefixes,"SELECT ?X ?Y WHERE { ?X <internal:PE> ?Y }", new HashMap[String,String](), System.out, "text/csv");

      // Close connection to RDFox
      RDFox.closeConnection(server,data)

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
       */
      val unsafe1 = for {
        axiom <- tbox
        if axiom.isT5
        role1 <- axiom.objectPropertyExpressionsInSignature
        roleSuper = role1 +: reasoner.superObjectProperties(role1).collect(Collectors.toList()).asScala
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