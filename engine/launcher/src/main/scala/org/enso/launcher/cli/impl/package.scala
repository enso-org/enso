package org.enso.launcher.cli

package object impl {
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

  def splitAtCharacter(char: Char)(line: String): (String, String) = {
    val index = line.indexOf(char.toString)
    if (index < 0) {
      ("", line)
    } else {
      (line.take(index), line.drop(index + 1))
    }
  }

  def alignTabulators(lines: Seq[String]): Seq[String] = {
    val splitted      = lines.map(splitAtCharacter('\t'))
    val prefixLengths = splitted.map(_._1.length)
    val padLength     = prefixLengths.max + 2
    def rightPad(str: String): String =
      if (str.length > padLength) str
      else {
        val padding = " " * (padLength - str.length)
        str + padding
      }

    splitted.map {
      case (prefix, suffix) => rightPad(prefix) + suffix
    }
  }
}
