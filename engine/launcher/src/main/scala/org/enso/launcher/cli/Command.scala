package org.enso.launcher.cli

case class CommandHelp(name: String, comment: String) {
  override def toString: String = s"$name\t$comment"
}
case class Command[A](name: String, comment: String)(
  val opts: Opts[A]
) {
  def help(applicationName: String): String = {
    val tableDivider = "\t"
    val usage =
      s"Usage: $applicationName $name$tableDivider${opts.commandLine()}\n\n"

    val options = opts.helpExplanations().mkString("\n")
    val optionsHelp =
      if (options.isEmpty) "" else "Available options:\n" + options + "\n\n"

    val additionalHelp = opts.additionalHelp().map(_ + "\n").mkString

    comment + "\n" + usage + optionsHelp + additionalHelp
  }

  def topLevelHelp: CommandHelp = CommandHelp(name, comment)
}
