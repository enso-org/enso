package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.enso.polyglot.runtime.Runtime.Api.ExpressionId

/** An object representing invalidated expressions selector.
  */
sealed trait InvalidatedExpressions
object InvalidatedExpressions {

  /** An object representing invalidation of all expressions.
    */
  @named("all")
  case class All() extends InvalidatedExpressions

  /** An object representing invalidation of a list of expressions.
    *
    * @param value a list of expressions to invalidate.
    */
  @named("expressions")
  case class Expressions(value: Vector[ExpressionId])
      extends InvalidatedExpressions
}
