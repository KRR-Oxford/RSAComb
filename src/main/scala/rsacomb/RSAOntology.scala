package rsacomb

/* Java imports */
import java.util.HashMap
import java.util.stream.{Collectors, Stream}

import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory

import tech.oxfordsemantic.jrdfox.client.{UpdateType, DataStoreConnection}
import tech.oxfordsemantic.jrdfox.logic.{Resource, Rule, Atom, Variable, IRI}

/* Scala imports */
import scala.collection.JavaConverters._
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

    /* Steps for RSA check
     * 1) convert ontology axioms into LP rules
     * 2) call RDFox on the onto and compute materialization
     * 3) build graph from E(x,y) facts
     * 4) check if the graph is tree-like
     *    ideally this annotates the graph with info about the reasons
     *    why the ontology might not be RSA. This could help a second
     *    step of approximation of an Horn-ALCHOIQ to RSA
     */
    def isRSA: Boolean = {

      val tbox = ontology.tboxAxioms(Imports.INCLUDED)
      val rbox = ontology.rboxAxioms(Imports.INCLUDED)
      val axioms =
        Stream
          .concat(tbox, rbox)
          .collect(Collectors.toList())
          .asScala
      val unsafe = ontology.getUnsafeRoles

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
          Variable.create("x"),
          SkolemStrategy.ConstantRSA(axiom.toString),
          unsafe
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

      /* Add ontology rules
       */
      data.addRules(datalog.asJava)

      /* Build graph
       */
      val graph = getRSAGraph(data);
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

    def getUnsafeRoles: List[OWLObjectPropertyExpression] = {
      // The reasoner is used to check unsafety condition for the ontology roles
      val factory = new StructuralReasonerFactory()
      val reasoner = factory.createReasoner(ontology)

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

      /* TODO: We should be able to avoid this last conversion to List.
       * Maybe we should just move everything to Sets instead of Lists,
       * since they have a more straightforward conversion from Java
       * collections.
       */
      (unsafe1 ++ unsafe2).toList
    }

    def getRSAGraph(
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

    def getFilteringProgram(query: Query): List[Rule] = {

      // Import implicit conversion to RDFox IRI
      import RDFoxUtil._

      sealed trait Reified;
      case class ReifiedHead(bind: BindAtom, atoms: List[Atom]) extends Reified
      case class ReifiedBody(atoms: List[Atom]) extends Reified
      case class Unaltered(formula: BodyFormula) extends Reified

      def getBindAtom(atom: Atom): BindAtom = {
        // TODO: We need to implement another way to introduce fresh
        // variables.
        val varA = Variable.create("A")
        val name =
          Literal.create(atom.getTupleTableName.getIRI, Datatype.XSD_STRING)
        val args = atom
          .getArguments()
          .asScala
          .toSeq
          .prepended(name)
        BindAtom.create(
          BuiltinFunctionCall
            .create("SKOLEM", args: _*),
          varA
        )
      }

      def reifyAtom(atom: Atom, variable: Variable): List[Atom] = {
        def iri(i: Int) = atom.getTupleTableName().getIRI() ++ s"_$i"
        atom
          .getArguments()
          .asScala
          .zipWithIndex
          .map { case (t, i) => Atom.rdf(variable, iri(i), t) }
          .toList
      }

      // Is this the best way to determine if an atom is an RDF triple?
      // Note that we can't use `getNumberOfArguments()` because is not
      // "consistent":
      // - for an atom created with `rdf(<term1>, <term2>, <term3>)`,
      // `getNumberOfArguments` returns 3
      // - for an atom created with `Atom.create(<tupletablename>, <term1>,
      // <term2>, <term3>)`, `getNumberOfArguments()` returns 3
      //
      // This is probably because `Atom.rdf(...) is implemented as:
      // ```scala
      //  def rdf(term1: Term, term2: Term, term3: Term): Atom =
      //    Atom.create(TupleTableName.create("internal:triple"), term1, term2, term3)
      // ```
      def isRdfTriple(atom: Atom): Boolean =
        atom.getTupleTableName.getIRI.equals("internal:triple")

      def reify(
          formula: BodyFormula,
          head: Boolean
      ): Reified = {
        def default[A <: BodyFormula](x: A) = Unaltered(x)
        formula match {
          case a: Atom => {
            if (!isRdfTriple(a)) {
              if (head) {
                val b = getBindAtom(a)
                ReifiedHead(b, reifyAtom(a, b.getBoundVariable))
              } else {
                val varA = Variable.create("A")
                ReifiedBody(reifyAtom(a, varA))
              }
            } else {
              default(a)
            }
          }
          case a => default(a)
        }
      }

      def skolemizeRule(rule: Rule): Rule = {
        // Rule body
        val body =
          rule.getBody.asScala.map(reify(_, false)).flatMap {
            case ReifiedHead(_, _) => List(); /* handle impossible case */
            case ReifiedBody(x)    => x;
            case Unaltered(x)      => List(x)
          }
        // Rule head
        val reified = rule.getHead.asScala.map(reify(_, true))
        val skols = reified.flatMap {
          case ReifiedHead(x, _) => Some(x);
          case ReifiedBody(_)    => None; /* handle impossible case */
          case Unaltered(_)      => None
        }
        val head = reified.flatMap {
          case ReifiedHead(_, x) => x;
          case ReifiedBody(_)    => List(); /* handle impossible case */
          case Unaltered(x) =>
            List(x.asInstanceOf[Atom]) /* Can we do better that a cast? */
        }
        Rule.create(head.asJava, (skols ++ body).asJava)
      }

      def formulaToRuleBody(body: Formula): List[BodyFormula] = {
        body match {
          case a: BodyFormula => List(a);
          case a: Conjunction =>
            a.getConjuncts().asScala.toList.flatMap(formulaToRuleBody(_));
          case _ => List() /* We don't handle this for now */
        }
      }

      val body = formulaToRuleBody(query.getQueryFormula)
      val vars: List[Term] = query.getAnswerVariables.asScala.toList
      def id(t1: Term, t2: Term) =
        Atom.create(
          TupleTableName.create("http://127.0.0.1/ID"),
          vars.appendedAll(List(t1, t2)).asJava
        )
      val qm = Atom.create(TupleTableName.create("QM"), vars.asJava)

      /* Filtering program */
      val rule1 = Rule.create(qm, body.asJava)
      val rule3a =
        for ((v, i) <- vars.zipWithIndex)
          yield Rule.create(
            id(
              IRI.create(s"http://127.0.0.1/$i"),
              IRI.create(s"http://127.0.0.1/$i")
            ),
            List(
              qm,
              Negation.create(
                Atom.rdf(v, IRI.RDF_TYPE, IRI.create("http://127.0.0.1/NI"))
              )
            ).asJava
          )
      val rule3b = Rule.create(
        id(Variable.create("V"), Variable.create("U")),
        id(Variable.create("U"), Variable.create("V"))
      )
      val rule3c = Rule.create(
        id(Variable.create("U"), Variable.create("W")),
        List[BodyFormula](
          id(Variable.create("U"), Variable.create("V")),
          id(Variable.create("V"), Variable.create("W"))
        ).asJava
      )

      var rules: List[Rule] =
        List.empty
          .prepended(rule3c)
          .prepended(rule3b)
          .prependedAll(rule3a)
          .prepended(rule1)

      // DEBUG
      println("FILTERING PROGRAM:")
      rules.map(skolemizeRule(_)).foreach(println(_))

      List()
    }

  } // implicit class RSAOntology

} // trait RSAOntology
