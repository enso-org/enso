package org.enso.launcher.cli

import java.nio.file.{InvalidPathException, Path}
import java.util.UUID

import cats.implicits._

trait Argument[A] {
  def read(string: String): Either[String, A]
}

object Argument {
  def apply[A](implicit argument: Argument[A]): Argument[A] = argument

  implicit val argumentString: Argument[String] =
    (string: String) => string.asRight

  implicit val argumentInteger: Argument[Int] =
    (string: String) =>
      try {
        string.toInt.asRight
      } catch {
        case _: NumberFormatException => s"Invalid number '$string'".asLeft
      }

  implicit val argumentPath: Argument[Path] =
    (string: String) =>
      try {
        Path.of(string).asRight
      } catch {
        case invalidPathException: InvalidPathException =>
          s"Invalid path '$string': ${invalidPathException.getMessage}".asLeft
      }

  implicit val argumentUUID: Argument[UUID] = (string: String) =>
    try { UUID.fromString(string).asRight }
    catch {
      case _: IllegalArgumentException | _: NumberFormatException =>
        s"Invalid UUID '$string'".asLeft
    }
}
