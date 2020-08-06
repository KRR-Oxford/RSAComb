package rsacomb

/* Java imports */
import org.semanticweb.owlapi.model.{OWLAxiom,OWLSubClassOfAxiom, OWLEquivalentClassesAxiom}
import org.semanticweb.owlapi.model.{OWLObjectPropertyExpression,OWLClass,OWLClassExpression,OWLObjectSomeValuesFrom,OWLObjectMaxCardinality}
import org.semanticweb.owlapi.model.ClassExpressionType
import org.semanticweb.owlapi.model.{OWLAxiomVisitorEx,OWLClassExpressionVisitorEx}

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
    case object T4 extends RSAAxiomType // A ⊑ ≤1R.B
    case object T5 extends RSAAxiomType // A ⊑ ∃R.B
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
      extends OWLAxiomVisitorEx[Boolean]
    {
      override
      def visit(axiom: OWLSubClassOfAxiom): Boolean = {
        val sub = axiom.getSubClass().getClassExpressionType()
        val sup = axiom.getSuperClass().getClassExpressionType()
        t match {
          case RSAAxiomType.T3 => // ∃R.A ⊑ B
            sub == ClassExpressionType.OBJECT_SOME_VALUES_FROM && sup == ClassExpressionType.OWL_CLASS
          case RSAAxiomType.T4 => // A ⊑ ≤1R.B
            sub == ClassExpressionType.OWL_CLASS && sup == ClassExpressionType.OBJECT_MAX_CARDINALITY
          case RSAAxiomType.T5 => // A ⊑ ∃R.B
            sub == ClassExpressionType.OWL_CLASS && sup == ClassExpressionType.OBJECT_SOME_VALUES_FROM
        }
      }

      override
      def visit(axiom: OWLEquivalentClassesAxiom): Boolean = {
        // TODO
        false
      }

      def doDefault(axiom : OWLAxiom): Boolean = false
    }

    private def isOfType(t: RSAAxiomType): Boolean = {
      val visitor = new RSAAxiomTypeDetector(t)
      axiom.accept(visitor)
    }

    /* Exposed methods */
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
    private class RSAAxiomRoleExtractor()
      extends OWLAxiomVisitorEx[List[OWLObjectPropertyExpression]]
    {

      private class RSAExprRoleExtractor()
        extends OWLClassExpressionVisitorEx[List[OWLObjectPropertyExpression]]
      {
        override
        def visit(expr: OWLObjectSomeValuesFrom): List[OWLObjectPropertyExpression] =
          List(expr.getProperty)

        override
        def visit(expr: OWLObjectMaxCardinality): List[OWLObjectPropertyExpression] =
          List(expr.getProperty)

        /* NOTE: this instance of `visit` for `OWLClass` shouldn't be necessary. However
         * if missing, the code throws a `NullPointerException`. It seems like, for some
         * reason, `OWLClass` is not really a subinterface of `OWLClassExpression`, as 
         * stated in the JavaDocs.
         */
        override
        def visit(expr: OWLClass): List[OWLObjectPropertyExpression] =
          List()

        def doDefault(expr: OWLClassExpression): List[OWLObjectPropertyExpression] =
          List()
      }

      override
      def visit(axiom: OWLSubClassOfAxiom): List[OWLObjectPropertyExpression] = {
        val visitor = new RSAExprRoleExtractor()
        val sub = axiom.getSubClass.accept(visitor)
        val sup = axiom.getSuperClass.accept(visitor)
        sub ++ sup
      }

      override
      def visit(axiom: OWLEquivalentClassesAxiom): List[OWLObjectPropertyExpression] = {
        // TODO
        List()
      }

      def doDefault(axiom : OWLAxiom): List[OWLObjectPropertyExpression] = List()
    }

    /* Exposed methods */
    def objectPropertyExpressionsInSignature: List[OWLObjectPropertyExpression] = {
      val visitor = new RSAAxiomRoleExtractor()
      axiom.accept(visitor)
    }
  }

} // trait RSAAxiom