package org.enso.launcher.cli

object Spelling {
  def selectClosestMatches(
    typo: String,
    possibleWords: Seq[String]
  ): Seq[String] = {
    val _ = typo
    possibleWords // TODO [RW] actual implementation
  }

  def suggestClosestMatches(
    typo: String,
    possibleWords: Seq[String]
  ): String = {
    val closestMatches = selectClosestMatches(typo, possibleWords)
    closestMatches match {
      case Seq()    => ""
      case Seq(one) => s" Did you mean '$one'?"
      case seq =>
        val quoted = seq.map(s => s"'$s'")
        val inits  = quoted.init.mkString(", ")
        s" Did you mean $inits or ${quoted.last}?"
    }
  }
}
