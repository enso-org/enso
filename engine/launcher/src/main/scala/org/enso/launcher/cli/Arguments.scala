package org.enso.launcher.cli

import nl.gn0s1s.bump.SemVer
import org.enso.cli.Argument

object Arguments {
  implicit val semverArgument: Argument[SemVer] = new Argument[SemVer] {
    override def read(string: String): Either[List[String], SemVer] =
      SemVer(string).toRight(List(s"`$string` is not a valid version string."))
  }
}
