package org.enso.launcher.cli

object CLIOutput {
  def alignAndWrap(text: String): String = {
    // TODO [RW]
    text
  }

  def println(text: String): Unit = {
    Predef.println(alignAndWrap(text))
  }
}
