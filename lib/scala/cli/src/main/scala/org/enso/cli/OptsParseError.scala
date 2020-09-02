package org.enso.cli

import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Semigroup

case class OptsParseError(
  errors: NonEmptyList[String],
  fullHelpRequested: Boolean = false,
  fullHelpAppended: Boolean  = false
) {
  def withErrors(additionalErrors: List[String]): OptsParseError =
    copy(errors = errors ++ additionalErrors)
  def withErrors(additionalErrors: String*): OptsParseError =
    withErrors(additionalErrors.toList)
  def shouldAppendShortHelp: Boolean = {
    val isHelpMentionedAlready = errors.exists(_.contains("--help"))
    val isHelpHandled =
      isHelpMentionedAlready || fullHelpAppended || fullHelpRequested
    !isHelpHandled
  }
  def withShortHelp(helpString: String): OptsParseError =
    withErrors(helpString)
  def withFullHelp(helpString: String): OptsParseError =
    withErrors(helpString).copy(
      fullHelpRequested = false,
      fullHelpAppended  = true
    )
}

object OptsParseError {
  def apply(error: String, errors: String*): OptsParseError =
    OptsParseError(NonEmptyList.of(error, errors: _*))

  def requestingFullHelp(error: String): OptsParseError =
    OptsParseError(NonEmptyList.one(error), fullHelpRequested = true)

  def left[A](error: String): Either[OptsParseError, A] = Left(apply(error))

  implicit val semigroup: Semigroup[OptsParseError] =
    (x: OptsParseError, y: OptsParseError) => {
      val fullHelpAppended = x.fullHelpAppended || y.fullHelpAppended
      OptsParseError(
        x.errors ++ y.errors.toList,
        if (fullHelpAppended) false
        else x.fullHelpRequested || y.fullHelpRequested,
        fullHelpAppended
      )
    }

  implicit class ParseErrorSyntax[A](val result: Either[OptsParseError, A]) {
    def addErrors(errors: List[String]): Either[OptsParseError, A] =
      result match {
        case Left(value) => Left(value.withErrors(errors))
        case Right(value) =>
          val nel = NonEmptyList.fromList(errors)
          nel match {
            case Some(errorsList) => Left(OptsParseError(errorsList))
            case None             => Right(value)
          }
      }

    def appendShortHelp(help: => String): Either[OptsParseError, A] =
      result.left.map { value =>
        if (value.shouldAppendShortHelp) value.withShortHelp(help) else value
      }

    def appendFullHelp(help: => String): Either[OptsParseError, A] =
      result.left.map { value =>
        if (value.fullHelpRequested) value.withFullHelp(help) else value
      }

    def toErrorList: Either[List[String], A] =
      result match {
        case Left(value) =>
          if (value.shouldAppendShortHelp || value.fullHelpRequested)
            throw new IllegalStateException(
              "Internal error: Help was not handled."
            )
          else
            Left(value.errors.toList)
        case Right(value) => Right(value)
      }
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
