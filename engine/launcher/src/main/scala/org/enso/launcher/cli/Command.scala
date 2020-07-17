package org.enso.launcher.cli

case class CommandHelp(name: String, comment: String) {
  override def toString: String = s"$name\t$comment"
}
case class Command[A](name: String, comment: String)(
  val opts: Opts[A]
) {
  def help(applicationName: String): String = {
    val tableDivider = "\t"
    val usages =
      opts.commandLines().map(s"$applicationName $name$tableDivider" + _)
    val firstLine = "Usage: "
    val padding   = " " * firstLine.length
    val usage =
      firstLine + usages.head +
      (usages.tail.map("\n" + padding + _)).mkString + "\n\n"

    val optionExplanations =
      Seq("[--help | -h]\tPrint this help message.") ++ opts.helpExplanations()
    val options = optionExplanations.map("    " + _).mkString("\n")
    val optionsHelp =
      if (options.isEmpty) "" else "Available options:\n" + options + "\n\n"

    val additionalHelp = opts.additionalHelp().map(_ + "\n").mkString

    comment + "\n" + usage + optionsHelp + additionalHelp
  }

  def topLevelHelp: CommandHelp = CommandHelp(name, comment)
}
