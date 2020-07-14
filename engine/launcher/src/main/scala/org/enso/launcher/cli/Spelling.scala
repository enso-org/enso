package org.enso.launcher.cli

object Spelling {
  def selectClosestMatches(
    typo: String,
    possibleWords: Seq[String]
  ): Seq[String] = {
    val _ = typo
    possibleWords // TODO [RW] actual implementation
  }
}
