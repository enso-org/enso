package org.enso.launcher.cli

case class CommandHelp(name: String, comment: String) {
  override def toString: String = s"$name\t$comment"
}
case class Command[Config](name: String, comment: String)(
  val opts: Opts[Config => Unit]
) {
  def help(applicationName: String): String = {
    val options = opts.helpExplanations().mkString("\n")
    val optionsHelp =
      if (options.isEmpty) "" else "Allowed options:\n" + options
    s"""$comment
       |Usage: $applicationName $name ${opts.commandLine()}
       |$optionsHelp
       |""".stripMargin
  }

  def topLevelHelp: CommandHelp = CommandHelp(name, comment)
}
