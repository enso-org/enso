package org.enso.cli

package object internal {
  def combineWithoutDuplicates[A](
    old: Either[List[String], Option[A]],
    newer: Either[List[String], A],
    duplicateErrorMessage: String
  ): Either[List[String], Option[A]] =
    (old, newer) match {
      case (Left(oldErrors), Left(newErrors)) => Left(newErrors ++ oldErrors)
      case (Right(None), Right(v))            => Right(Some(v))
      case (Right(Some(_)), Right(_))         => Left(List(duplicateErrorMessage))
      case (Left(errors), Right(_))           => Left(errors)
      case (Right(_), Left(errors))           => Left(errors)
    }
}
