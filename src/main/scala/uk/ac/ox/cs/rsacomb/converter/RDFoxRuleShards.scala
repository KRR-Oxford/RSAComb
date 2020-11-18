package uk.ac.ox.cs.rsacomb.converter

import tech.oxfordsemantic.jrdfox.logic.datalog.{TupleTableAtom, BodyFormula}

case class RDFoxRuleShards(res: List[TupleTableAtom], ext: List[BodyFormula])
