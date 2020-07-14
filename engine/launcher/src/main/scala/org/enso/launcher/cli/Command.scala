package org.enso.launcher.cli

case class CommandHelp(name: String, comment: String) {
  override def toString: String = s"$name\t$comment"
}
case class Command(name: String, comment: String)(val opts: Opts[() => Unit]) {
  def help(applicationName: String): String = {
    val explanations = opts.helpExplanations().mkString("\n")
    s"""$comment
       |Usage: $applicationName $name ${opts.commandLine()}
       |$explanations
       |""".stripMargin
  }

  def topLevelHelp: CommandHelp = CommandHelp(name, comment)
}
