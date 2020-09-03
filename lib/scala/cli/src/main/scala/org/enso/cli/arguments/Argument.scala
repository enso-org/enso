package org.enso.cli.arguments

import java.nio.file.{InvalidPathException, Path}
import java.util.UUID

import cats.implicits._

/**
  * A typeclass which defines logic of parsing a String into a value of type A.
  */
trait Argument[A] {

  /**
    * Tries to convert the given string into a value of type A.
    */
  def read(string: String): Either[OptsParseError, A]
}

object Argument {
  def apply[A](implicit argument: Argument[A]): Argument[A] = argument

  /**
    * [[Argument]] instance that just returns the provided [[String]].
    */
  implicit val argumentString: Argument[String] =
    (string: String) => string.asRight

  /**
    * [[Argument]] instance that tries to treat the String as an [[Int]].
    */
  implicit val argumentInteger: Argument[Int] =
    (string: String) =>
      try {
        string.toInt.asRight
      } catch {
        case _: NumberFormatException =>
          OptsParseError.left(s"Invalid number `$string`")
      }

  /**
    * [[Argument]] instance that tries to parse the String as a [[Path]].
    */
  implicit val argumentPath: Argument[Path] =
    (string: String) =>
      try {
        Path.of(string).asRight
      } catch {
        case invalidPathException: InvalidPathException =>
          OptsParseError.left(
            s"Invalid path `$string`: ${invalidPathException.getMessage}"
          )
      }

  /**
    * [[Argument]] instance that tries to parse the String as a [[UUID]].
    */
  implicit val argumentUUID: Argument[UUID] = (string: String) =>
    try { UUID.fromString(string).asRight }
    catch {
      case _: IllegalArgumentException | _: NumberFormatException =>
        OptsParseError.left(s"Invalid UUID `$string`")
    }
}
