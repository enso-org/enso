package org.enso.compiler.core.ir

/** A trait representing the classification of IR nodes into either primitive
  * (constructs which will remain after desugaring) or sugar (constructs that
  * should be removed by the desugaring passes).
  */
sealed trait IRKind

object IRKind {

  /** This trait encodes that a given piece of the [[IR]] is considered to be
    * a primitive construct in Enso.
    */
  trait Primitive extends IRKind

  /** This trait encodes that a given piece of the [[IR]] is considered to
    * represent syntax sugar in Enso.
    *
    * All [[Sugar]] constructs should be desugared into [[Primitive]]
    * constructs as soon as possible.
    */
  trait Sugar extends IRKind

  /** This trait encodes that a given piece of [[IR]] is used to represent an
    * optimisation on the IR in Enso.
    */
  trait Optimisation extends IRKind
}
