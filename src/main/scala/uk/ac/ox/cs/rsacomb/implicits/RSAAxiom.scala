package uk.ac.ox.cs.rsacomb.implicits

/* Java imports */
import org.semanticweb.owlapi.model.{
  OWLAxiom,
  OWLSubClassOfAxiom,
  OWLEquivalentClassesAxiom
}
import org.semanticweb.owlapi.model.{
  OWLObjectPropertyExpression,
  OWLSubObjectPropertyOfAxiom,
  OWLClass,
  OWLClassExpression,
  OWLObjectSomeValuesFrom,
  OWLObjectMaxCardinality
}
import org.semanticweb.owlapi.model.ClassExpressionType
import org.semanticweb.owlapi.model.{
  OWLAxiomVisitorEx,
  OWLClassExpressionVisitorEx
}
import org.semanticweb.owlapi.model.OWLObjectProperty
import scala.collection.JavaConverters._

/* Wrapper trait for the implicit class `RSAAxiom`.
 */
trait RSAAxiom {

  /* Identifies some of the axiom types in a Horn-ALCHOIQ ontology
   * in normal form. Refer to the paper for more details on the
   * chosen names.
   */
  private sealed trait RSAAxiomType
  private object RSAAxiomType {
    case object T3 extends RSAAxiomType // ∃R.A ⊑ B
    case object T3top extends RSAAxiomType // ∃R.⊤ ⊑ B
    case object T4 extends RSAAxiomType // A ⊑ ≤1R.B
    case object T5 extends RSAAxiomType // A ⊑ ∃R.B
  }

  object RSAAxiom {

    def hashed(
        cls1: OWLClass,
        prop: OWLObjectPropertyExpression,
        cls2: OWLClass
    ): String =
      (cls1, prop, cls2).hashCode.toString

  }

  /* Implements additional features on top of `OWLAxiom` from
   * the OWLAPI.
   */
  implicit class RSAAxiom(axiom: OWLAxiom) {

    /* Detecting axiom types:
     *
     * In order to reason about role unsafety in Horn-ALCHOIQ
     * ontologies we need to detect and filter axioms by their
     * "type".
     *
     * This is a simple implementation following the Visitor
     * pattern imposed by the OWLAPI.
     */
    private class RSAAxiomTypeDetector(t: RSAAxiomType)
        extends OWLAxiomVisitorEx[Boolean] {
      override def visit(axiom: OWLSubClassOfAxiom): Boolean = {
        val sub = axiom.getSubClass().getClassExpressionType()
        val sup = axiom.getSuperClass().getClassExpressionType()
        t match {
          case RSAAxiomType.T3top => // ∃R.⊤ ⊑ B
            axiom.isT3 && axiom
              .getSubClass()
              .asInstanceOf[OWLObjectSomeValuesFrom]
              .getFiller
              .isOWLThing
          case RSAAxiomType.T3 => // ∃R.A ⊑ B
            sub == ClassExpressionType.OBJECT_SOME_VALUES_FROM && sup == ClassExpressionType.OWL_CLASS
          case RSAAxiomType.T4 => // A ⊑ ≤1R.B
            sub == ClassExpressionType.OWL_CLASS && sup == ClassExpressionType.OBJECT_MAX_CARDINALITY
          case RSAAxiomType.T5 => // A ⊑ ∃R.B
            sub == ClassExpressionType.OWL_CLASS && sup == ClassExpressionType.OBJECT_SOME_VALUES_FROM
        }
      }

      override def visit(axiom: OWLEquivalentClassesAxiom): Boolean = {
        // TODO
        false
      }

      def doDefault(axiom: OWLAxiom): Boolean = false
    }

    private def isOfType(t: RSAAxiomType): Boolean = {
      val visitor = new RSAAxiomTypeDetector(t)
      axiom.accept(visitor)
    }

    /* Exposed methods */
    def isT3top: Boolean = isOfType(RSAAxiomType.T3top)
    def isT3: Boolean = isOfType(RSAAxiomType.T3)
    def isT4: Boolean = isOfType(RSAAxiomType.T4)
    def isT5: Boolean = isOfType(RSAAxiomType.T5)

    /* Extracting ObjectPropertyExpressions from axioms
     *
     * This extracts all ObjectPropertyExpressions from a given
     * axiom. While the implementation is generic we use it on axioms
     * of specific types (see above).
     *
     * NOTE: it is not possible to use the `objectPropertyInSignature`
     * method of `OWLAxiom` because it returns all "role names" involved
     * in the signature of an axiom. In particular we won't get the inverse
     * of a role if this appears in the axiom (but we will get the role
     * itself instead).
     */
    lazy val objectPropertyExpressionsInSignature
        : List[OWLObjectPropertyExpression] =
      axiom match {
        case a: OWLSubClassOfAxiom =>
          rolesInExpr(a.getSubClass) ++ rolesInExpr(a.getSuperClass)
        case a: OWLEquivalentClassesAxiom =>
          a.getClassExpressions.asScala.toList.flatMap(rolesInExpr(_))
        case a: OWLSubObjectPropertyOfAxiom =>
          List(a.getSubProperty, a.getSuperProperty)
        case _ => List()
      }

    private def rolesInExpr(
        expr: OWLClassExpression
    ): List[OWLObjectPropertyExpression] =
      expr match {
        case e: OWLObjectSomeValuesFrom => List(e.getProperty)
        case e: OWLObjectMaxCardinality => List(e.getProperty)
        case _                          => List()
      }

    lazy val toTriple: Option[(OWLClass, OWLObjectProperty, OWLClass)] =
      for {
        subClass <- Some(axiom) collect { case a: OWLSubClassOfAxiom => a }
        cls1 <- Some(subClass.getSubClass) collect { case a: OWLClass => a }
        someValues <- Some(subClass.getSuperClass) collect {
          case a: OWLObjectSomeValuesFrom => a
        }
        prop <- Some(someValues.getProperty) collect {
          case a: OWLObjectProperty => a
        }
        cls2 <- Some(someValues.getFiller) collect { case a: OWLClass => a }
      } yield (cls1, prop, cls2)

    lazy val hashed: String = (RSAAxiom.hashed _) tupled toTriple.get
  }

} // trait RSAAxiom