package org.enso.polyglot.data

import com.github.plokhotnyuk.jsoniter_scala.macros.named

/** An either-or-both data type. */
sealed trait These[+A, +B]

object These {

  @named("theseHere")
  case class Here[+A](
    here: A
  ) extends These[A, Nothing]

  @named("theseThere")
  case class There[+B](
    there: B
  ) extends These[Nothing, B]

  @named("theseBoth")
  case class Both[+A, +B](
    here: A,
    there: B
  ) extends These[A, B]
}
