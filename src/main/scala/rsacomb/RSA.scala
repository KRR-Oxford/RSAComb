package rsacomb.util

/* Java imports */
import java.util.Map

import tech.oxfordsemantic.jrdfox.formats.SPARQLParser
import tech.oxfordsemantic.jrdfox.Prefixes
import tech.oxfordsemantic.jrdfox.logic.datalog.{
  TupleTableAtom,
  TupleTableName,
  Negation
}
import tech.oxfordsemantic.jrdfox.logic.expression.{Term, Variable, IRI}
import org.semanticweb.owlapi.model.OWLOntology
import org.semanticweb.owlapi.model.{
  OWLAxiom,
  OWLClass,
  OWLObjectPropertyExpression
}

import rsacomb.suffix.RSASuffix

// Debug only
import scala.collection.JavaConverters._

object RSA {

  val Prefixes: Prefixes = new Prefixes()
  Prefixes.declarePrefix("rsa:", "http://127.0.0.1/")

  private def atom(name: IRI, vars: List[Term]) =
    TupleTableAtom.create(TupleTableName.create(name.getIRI), vars: _*)

  def PE(t1: Term, t2: Term) =
    TupleTableAtom.rdf(t1, RSA("PE"), t2)

  def U(t: Term) =
    TupleTableAtom.rdf(t, IRI.RDF_TYPE, RSA("U"))

  def In(t: Term)(implicit set: Term) =
    TupleTableAtom.rdf(t, RSA("In"), set)

  def notIn(t: Term)(implicit set: Term) = Negation.create(In(t)(set))

  def EquivTo(t1: Term, t2: Term) =
    TupleTableAtom.rdf(t1, RSA("EquivTo"), t2)

  def QM(implicit variables: (List[Term], List[Term])) = {
    val (answer, bounded) = variables
    atom(RSA("QM"), answer ::: bounded)
  }

  def ID(t1: Term, t2: Term)(implicit variables: (List[Term], List[Term])) = {
    val (answer, bounded) = variables
    atom(RSA("ID"), (answer ::: bounded) :+ t1 :+ t2)
  }

  def Named(t: Term) =
    TupleTableAtom.rdf(t, IRI.RDF_TYPE, RSA("Named"))

  def Thing(t: Term) =
    TupleTableAtom.rdf(t, IRI.RDF_TYPE, IRI.THING)

  def NI(t: Term) =
    TupleTableAtom.rdf(t, IRI.RDF_TYPE, RSA("NI"))

  def TQ(t1: Term, t2: Term, sx: RSASuffix)(implicit
      variables: (List[Term], List[Term])
  ) = {
    val (answer, bounded) = variables
    atom(RSA("TQ" :: sx), (answer ::: bounded) :+ t1 :+ t2)
  }

  def AQ(t1: Term, t2: Term, sx: RSASuffix)(implicit
      variables: (List[Term], List[Term])
  ) = {
    val (answer, bounded) = variables
    atom(RSA("AQ" :: sx), (answer ::: bounded) :+ t1 :+ t2)
  }

  def FK(implicit variables: (List[Term], List[Term])) = {
    val (answer, bounded) = variables
    atom(RSA("FK"), answer ::: bounded)
  }

  def SP(implicit variables: (List[Term], List[Term])) = {
    val (answer, bounded) = variables
    atom(RSA("SP"), answer ::: bounded)
  }

  def Ans(implicit variables: (List[Term], List[Term])) = {
    val (answer, _) = variables
    atom(RSA("Ans"), answer)
  }

  def apply(name: Any): IRI =
    IRI.create(
      Prefixes.getPrefixIRIsByPrefixName.get("rsa:").getIRI + name.toString
    )
}
