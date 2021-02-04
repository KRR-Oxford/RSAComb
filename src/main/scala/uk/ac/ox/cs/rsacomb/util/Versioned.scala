package uk.ac.ox.cs.rsacomb.util

/** Utility to allow hussle free version switching of blocks of code
  *
  * This allows for example testing different implementations of a
  * module or algorithm.
  */
trait Versioned[T] {

  /** Type of the returned versioned object */
  type Result

  /** Returns correct instance of the versioned object
    *
    * @param t object uniquely identifing the requested instance.
    */
  def apply(t: T): Result
}
