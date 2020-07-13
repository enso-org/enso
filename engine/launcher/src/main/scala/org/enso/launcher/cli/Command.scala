package org.enso.launcher.cli

case class Command(name: String, comment: String)(val opts: Opts[Unit]) {
  def help: String = "TODO help" // deep help
}

sealed trait PluginBehaviour
case object PluginNotFound        extends PluginBehaviour
case object PluginInterceptedFlow extends PluginBehaviour
class Commands(
  val commands: Seq[Command],
  val plugins: Option[(String, Seq[String]) => PluginBehaviour]
)
object Commands {
  def apply(commands: Seq[Command]): Commands = new Commands(commands, None)
  def apply(
    commands: Seq[Command],
    pluginHandler: (String, Seq[String]) => PluginBehaviour
  ): Commands = new Commands(commands, Some(pluginHandler))

  def parse(commands: Commands)(
    args: Seq[String]
  ): Either[List[String], Unit] = ???

  def help(subcommands: Seq[Command]): String = ??? // top-level help
}
