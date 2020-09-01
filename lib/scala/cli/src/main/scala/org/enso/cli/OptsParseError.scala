package org.enso.cli

import cats.data.NonEmptyList
import cats.kernel.Semigroup

case class OptsParseError(errors: NonEmptyList[String], appendHelp: Boolean)

object OptsParseError {
  implicit val semigroup: Semigroup[OptsParseError] =
    (x: OptsParseError, y: OptsParseError) =>
      OptsParseError(
        x.errors ++ y.errors.toList,
        x.appendHelp || y.appendHelp
      )

  def product[A, B](
    a: Either[List[String], A],
    b: Either[List[String], B]
  ): Either[List[String], (A, B)] =
    (a, b) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(a), Left(b))   => Left(a ++ b)
      case (Left(a), _)         => Left(a)
      case (_, Left(b))         => Left(b)
    }
}
