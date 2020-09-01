package org.enso.cli

import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.implicits._

case class OptsParseError(
  errors: NonEmptyList[String],
  appendHelp: Boolean = false
) {
  def withErrors(additionalErrors: List[String]): OptsParseError =
    copy(errors = errors ++ additionalErrors)
  def withErrors(additionalErrors: String*): OptsParseError =
    withErrors(additionalErrors.toList)
  def withHelp(helpString: String): OptsParseError =
    withErrors(List(helpString)).copy(appendHelp = false)
}

object OptsParseError {
  def apply(error: String, errors: String*): OptsParseError =
    OptsParseError(NonEmptyList.of(error, errors: _*))

  def left[A](error: String): Either[OptsParseError, A] = Left(apply(error))

  implicit val semigroup: Semigroup[OptsParseError] =
    (x: OptsParseError, y: OptsParseError) =>
      OptsParseError(
        x.errors ++ y.errors.toList,
        x.appendHelp || y.appendHelp
      )

  def addErrors[A](
    result: Either[OptsParseError, A],
    errors: List[String]
  ): Either[OptsParseError, A] =
    result match {
      case Left(value) => Left(value.withErrors(errors))
      case Right(value) =>
        val nel = NonEmptyList.fromList(errors)
        nel match {
          case Some(errorsList) => Left(OptsParseError(errorsList))
          case None             => Right(value)
        }
    }

  def appendHelp[A](
    result: Either[OptsParseError, A]
  )(help: => String): Either[OptsParseError, A] =
    result.left.map { value =>
      if (value.appendHelp) value.withHelp(help) else value
    }

  def toErrorListAssumingHelpIsHandled[A](
    result: Either[OptsParseError, A]
  ): Either[List[String], A] =
    result match {
      case Left(value) =>
        if (value.appendHelp)
          throw new IllegalStateException(
            "Internal error: Help was not handled."
          )
        else
          Left(value.errors.toList)
      case Right(value) => Right(value)
    }

  def product[A, B](
    a: Either[OptsParseError, A],
    b: Either[OptsParseError, B]
  ): Either[OptsParseError, (A, B)] =
    (a, b) match {
      case (Right(a), Right(b)) => Right((a, b))
      case (Left(a), Left(b))   => Left(a |+| b)
      case (Left(a), _)         => Left(a)
      case (_, Left(b))         => Left(b)
    }
}
