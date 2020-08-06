package rsacomb

/* Java imports */
// import java.io.File
// import java.util.stream.{Collectors,Stream}

// import org.semanticweb.owlapi.apibinding.OWLManager
// import org.semanticweb.owlapi.model.{OWLOntologyManager,OWLOntology}
// import org.semanticweb.owlapi.model.{OWLAxiom,OWLObjectPropertyExpression}
import org.semanticweb.owlapi.model.{OWLAxiom,OWLSubClassOfAxiom, OWLEquivalentClassesAxiom}
import org.semanticweb.owlapi.model.OWLAxiomVisitorEx
// import org.semanticweb.owlapi.model.parameters.Imports
// import org.semanticweb.owlapi.reasoner.OWLReasoner
// import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

// import tech.oxfordsemantic.jrdfox.logic.Variable

/* Scala imports */
// import scala.collection.JavaConverters._

/* Local imports */
// import rsacomb.RSAAxiom 

/* Debug only */
// import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer

// import java.util.HashMap
// import java.util.stream.{Stream,Collectors}

// import org.semanticweb.owlapi.model.{AxiomType, ClassExpressionType, OWLObjectSomeValuesFrom}
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
//import org.semanticweb.owlapi.model.{OWLAxiom,OWLObjectPropertyExpression}

trait RSAAxiom {

  sealed trait RSAAxiomType
  object RSAAxiomType {
    case object T3 extends RSAAxiomType
    case object T4 extends RSAAxiomType
    case object T5 extends RSAAxiomType
  }

  implicit class RSAAxiom(axiom: OWLAxiom) {

    private class RSAAxiomTypeDetector(t: RSAAxiomType)
      extends OWLAxiomVisitorEx[Boolean]
    {

      override
      def visit(axiom: OWLSubClassOfAxiom): Boolean = {
        true
      }

      override
      def visit(axiom: OWLEquivalentClassesAxiom): Boolean = {
        true
      }

      def doDefault(axiom : OWLAxiom): Boolean = false

    }

    private def isOfType(t: RSAAxiomType): Boolean = {
      val visitor = new RSAAxiomTypeDetector(t)
      axiom.accept(visitor)
    }

    def isT3: Boolean = isOfType(RSAAxiomType.T3)
    def isT4: Boolean = isOfType(RSAAxiomType.T4)
    def isT5: Boolean = isOfType(RSAAxiomType.T5)
  }

}
