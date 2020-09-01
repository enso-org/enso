package org.enso.cli

import cats.implicits._

package object internal {
  def combineWithoutDuplicates[A](
    old: Either[OptsParseError, Option[A]],
    newer: Either[OptsParseError, A],
    duplicateErrorMessage: String
  ): Either[OptsParseError, Option[A]] =
    (old, newer) match {
      case (Left(oldErrors), Left(newErrors)) => Left(newErrors |+| oldErrors)
      case (Right(None), Right(v))            => Right(Some(v))
      case (Right(Some(_)), Right(_)) =>
        Left(OptsParseError(duplicateErrorMessage))
      case (Left(errors), Right(_)) => Left(errors)
      case (Right(_), Left(errors)) => Left(errors)
    }
}
