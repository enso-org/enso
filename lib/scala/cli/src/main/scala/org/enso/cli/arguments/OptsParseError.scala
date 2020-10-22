package org.enso.cli.arguments

import cats.data.NonEmptyList
import cats.implicits._
import cats.kernel.Semigroup

/** Aggregates errors encountered when parsing [[Opts]] and allows to attach
  * help texts.
  *
  * @param errors list of parse errors
  * @param fullHelpRequested specifies if attaching a full help text was
  *                          requested
  * @param fullHelpAppended specifies if a full help text has already been
  *                         attached
  */
case class OptsParseError(
  errors: NonEmptyList[String],
  fullHelpRequested: Boolean = false,
  fullHelpAppended: Boolean  = false
) {

  /** Creates a copy with additional errors from the provided list.
    */
  def withErrors(additionalErrors: List[String]): OptsParseError =
    copy(errors = errors ++ additionalErrors)

  /** Creates a copy with additional errors.
    */
  def withErrors(additionalErrors: String*): OptsParseError =
    withErrors(additionalErrors.toList)

  /** Specifies if short help should be appended.
    *
    * Short help is appended if it was not already mentioned in any of the
    * errors and if the full help is not added (or requested to be added later).
    */
  def shouldAppendShortHelp: Boolean = {
    val isHelpMentionedAlready = errors.exists(_.contains("--help"))
    val isHelpHandled =
      isHelpMentionedAlready || fullHelpAppended || fullHelpRequested
    !isHelpHandled
  }

  /** Creates a copy with short help appended.
    */
  def withShortHelp(helpString: String): OptsParseError =
    withErrors(helpString)

  /** Creates a copy with full help appended.
    */
  def withFullHelp(helpString: String): OptsParseError =
    withErrors(helpString).copy(
      fullHelpRequested = false,
      fullHelpAppended  = true
    )
}

object OptsParseError {

  /** Creates a parse error containing the provided errors.
    */
  def apply(error: String, errors: String*): OptsParseError =
    OptsParseError(NonEmptyList.of(error, errors: _*))

  /** Creates a parse error containing the provided error and indicating that
    * full help text should be appended to it.
    */
  def requestingFullHelp(error: String): OptsParseError =
    OptsParseError(NonEmptyList.one(error), fullHelpRequested = true)

  /** Helper method that creates a parse error and wraps it in a [[Left]].
    */
  def left[A](error: String): Either[OptsParseError, A] = Left(apply(error))

  /** [[Semigroup]] instance for [[OptsParseError]] that allows to merge
    * multiple errors into one.
    */
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

  /** Syntax extensions that add helper functions to objects of type
    * `Either[OptsParseError, A]` (parsing results).
    */
  implicit class ParseErrorSyntax[A](val result: Either[OptsParseError, A]) {

    /** Add errors from the specified list to the result.
      *
      * If the result was [[Left]], the errors are appended. If the result was
      * [[Right]] and the list is non-empty, the modified result is [[Left]]
      * containing the new errors.
      */
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

    /** Appends the provided short help if it should be added.
      */
    def appendShortHelp(help: => String): Either[OptsParseError, A] =
      result.left.map { value =>
        if (value.shouldAppendShortHelp) value.withShortHelp(help) else value
      }

    /** Appends the provided full help text if it was requested.
      */
    def appendFullHelp(help: => String): Either[OptsParseError, A] =
      result.left.map { value =>
        if (value.fullHelpRequested) value.withFullHelp(help) else value
      }

    /** Converts to an [[Either]] containing just a list of errors on failure.
      *
      * Makes sure that all help requests have been handled.
      */
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

  /** Merges two parse results into one that returns the pair created from
    * arguments' results on success.
    *
    * If any of the arguments is failed, the result is also a failure. If both
    * are failed, their errors are merged.
    */
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

  /** Combines two parse results ensuring that the old one was not set already.
    *
    * If any of the results is failed, the final result is also a failure. If
    * both results are successful and the first one is empty, the second one is
    * returned; if the first one is non-empty, the duplicate error is reported.
    *
    * This helper is used to implement parameters that should hold only one
    * result.
    */
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
