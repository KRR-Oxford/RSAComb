package rsacomb

import tech.oxfordsemantic.jrdfox.logic.{Atom, IRI, TupleTableName}

trait RDFTriple {

  implicit class RDFTriple(atom: Atom) {

    /* Is this the best way to determine if an atom is an RDF triple?
     * Note that we can't use `getNumberOfArguments()` because is not
     * "consistent":
     * - for an atom created with `rdf(<term1>, <term2>, <term3>)`,
     * `getNumberOfArguments` returns 3
     * - for an atom created with `Atom.create(<tupletablename>, <term1>,
     * <term2>, <term3>)`, `getNumberOfArguments()` returns 3
     *
     * This is probably because `Atom.rdf(...) is implemented as:
     * ```scala
     *  def rdf(term1: Term, term2: Term, term3: Term): Atom =
     *    Atom.create(TupleTableName.create("internal:triple"), term1, term2, term3)
     * ```
     */
    def isRdfTriple: Boolean =
      atom.getTupleTableName.getIRI.equals("internal:triple")

    def isClassAssertion: Boolean =
      atom.isRdfTriple && atom.getArgument(1).equals(IRI.RDF_TYPE)

    def isRoleAssertion: Boolean =
      atom.isRdfTriple && !atom.getArgument(1).equals(IRI.RDF_TYPE)

    def suffix(sx: String): Atom =
      if (this.isClassAssertion) {
        val newclass = atom.getArgument(2) match {
          case iri: IRI => IRI.create(iri.getIRI.appendedAll(sx))
          case other    => other
        }
        Atom.rdf(
          atom getArgument 0,
          atom getArgument 1,
          newclass
        )
      } else if (this.isRoleAssertion) {
        val newrole = atom.getArgument(1) match {
          case iri: IRI => IRI.create(iri.getIRI.appendedAll(sx))
          case other    => other
        }
        Atom.rdf(
          atom getArgument 0,
          newrole,
          atom getArgument 2
        )
      } else {
        val newname =
          TupleTableName.create(atom.getTupleTableName.getIRI.appendedAll(sx))
        Atom.create(newname, atom.getArguments())
      }
  }

}
