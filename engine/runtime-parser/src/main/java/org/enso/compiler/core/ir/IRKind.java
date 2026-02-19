package org.enso.compiler.core.ir;

/**
 * A trait representing the classification of IR nodes into either primitive (constructs which will
 * remain after desugaring) or sugar (constructs that should be removed by the desugaring passes).
 */
public sealed interface IRKind permits IRKind.Primitive, IRKind.Sugar, IRKind.Optimisation {
  /**
   * This trait encodes that a given piece of the {@code IR} is considered to be a primitive
   * construct in Enso.
   */
  non-sealed interface Primitive extends IRKind {}

  /**
   * This trait encodes that a given piece of the {@code IR} is considered to represent syntax sugar
   * in Enso.
   *
   * <p>All {@code Sugar} constructs should be desugared into {@code Primitive} constructs as soon
   * as possible.
   */
  non-sealed interface Sugar extends IRKind {}

  /**
   * This trait encodes that a given piece of {@code IR} is used to represent an optimisation on the
   * IR in Enso.
   */
  non-sealed interface Optimisation extends IRKind {}
}
