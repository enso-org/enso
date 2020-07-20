package org.enso.launcher.cli

case class CommandHelp(name: String, comment: String) {
  override def toString: String = s"$name\t$comment"
}
case class Command[A](
  name: String,
  comment: String,
  related: Seq[String] = Seq()
)(
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
    val options = optionExplanations.map(CLIOutput.indent + _).mkString("\n")
    val optionsHelp =
      if (options.isEmpty) "" else "Available options:\n" + options + "\n\n"

    val additionalHelp = opts.additionalHelp().map(_ + "\n").mkString

    comment + "\n" + usage + optionsHelp + additionalHelp
  }

  def topLevelHelp: CommandHelp = CommandHelp(name, comment)
}

object Command {
  def relatedMapping(commands: Seq[Command[_]]): Map[String, String] = {
    Map.from(commands.flatMap(cmd => cmd.related.map(_ -> cmd.name)))
  }

  def formatRelated(
    name: String,
    commandsPath: Seq[String],
    availableCommands: Seq[Command[_]]
  ): Option[String] = {
    val mapping = relatedMapping(availableCommands)
    for {
      related <- mapping.get(name)
      command = commandsPath.mkString(" ")
    } yield s"You may be looking for `$command $related`.\n\n" +
    s"To show usage, run `$command $related --help`.\n" +
    s"To show available commands, run `$command --help`."
  }
}
