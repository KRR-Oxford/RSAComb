package uk.ac.ox.cs.rsacomb.sparql

import tech.oxfordsemantic.jrdfox.logic.expression.{Resource, Variable}

/** A collections of answers to a query.
  *
  * Will take the query type (CQ or BCQ) into consideration when
  * serialised.
  *
  * @param bcq whether the answers are the result of a BCQ.
  * @param answers collection of answers to a query. When dealing with
  * BCQs, and empty collection represents a ''false'', ''true'' otherwise.
  */
class ConjunctiveQueryAnswers(
    bcq: Boolean,
    val variables: Seq[Variable],
    val answers: Seq[Seq[Resource]]
) {

  override def toString(): String =
    if (bcq) {
      if (answers.isEmpty) "FALSE" else "TRUE"
    } else {
      if (answers.isEmpty)
        "NO ANSWER."
      else {
        val header = variables map (_.getName) mkString "\t"
        val body = answers.map(_.mkString("\t")).mkString("\n")
        s"$header\n$body"
      }
    }
}
