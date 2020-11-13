package org.enso.cli.arguments

/** Represents a top-level command in the CLI.
  *
  * @param name name of the command
  * @param comment a help comment displayed in the commands help text
  * @param related names of related command names that should redirect the user
  *                to this command
  * @param opts parsing logic for command's options
  * @tparam A type returned by the command
  */
case class Command[A](
  name: String,
  comment: String,
  opts: Opts[A],
  related: Seq[String]
) {

  /** Returns a top-level help entry for the application help text. It includes
    * a short description of the command.
    */
  def topLevelHelp: CommandHelp = CommandHelp(name, comment)
}

object Command {

  /** Utility constructor for creating commands.
    *
    * Allows for the following syntax
    * {{{
    *   val command = Command("name", "help text.") {
    *     Opts.positionalArgument[Int]("ARG") map { arg =>
    *       println(s"Argument was $arg.")
    *     }
    *   }
    * }}}
    */
  def apply[A](name: String, comment: String, related: Seq[String] = Seq())(
    opts: Opts[A]
  ): Command[A] = new Command(name, comment, opts, related)

  /** A helper function that creates a mapping of related command names from a
    * list of commands. The mapping maps related command names to the original
    * commands that they should point to.
    */
  def relatedMapping(commands: Seq[Command[_]]): Map[String, String] = {
    Map.from(commands.flatMap(cmd => cmd.related.map(_ -> cmd.name)))
  }

  /** Creates a message that is displayed when a related command is executed. It
    * points the user to the original command instead and informs them on usage.
    *
    * @param name name of the entered, unkown command
    * @param commandsPath a sequence of words used to generate the help command
    * @param availableCommands a sequence of available subcommands that is
    *                          checked for related commands
    * @return if `name` matches a related command name in one of the
    *         `availableCommands`, an explanation message is returned, pointing
    *         the user to the original command. If the `name` does not match any
    *         names, None is returned.
    */
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
