package org.enso.polyglot.debugger

import org.scalatest.exceptions.{StackDepthException, TestFailedException}
import org.scalactic.source

trait EitherValue {
  implicit def convertEitherToRightValueHelper[A, B](
    either: Either[A, B]
  )(implicit pos: source.Position): EitherRightValueHelper[A, B] =
    new EitherRightValueHelper(either, pos)

  class EitherRightValueHelper[A, B](
    either: Either[A, B],
    pos: source.Position
  ) {
    def rightValue: B = either match {
      case Right(value) => value
      case Left(_) =>
        throw new TestFailedException({ _: StackDepthException =>
          Some(s"Either right value was expected, but it was $either")
        }, None, pos)
    }
  }
}
