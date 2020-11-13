package org.enso.cli.arguments

/** A plugin manager that handles finding and running plugins.
  */
trait PluginManager {

  /** Tries to run a given plugin with provided arguments.
    *
    * It never returns - either it exits with the plugin's exit code or throws
    * an exception if it was not possible to run the plugin.
    *
    * @param name name of the plugin
    * @param args arguments that should be passed to it
    * @return exit code of the launched plugin
    */
  def runPlugin(name: String, args: Seq[String]): Int

  /** Returns whether the plugin of the given `name` is available in the system.
    */
  def hasPlugin(name: String): Boolean

  /** Lists names of plugins found in the system.
    */
  def pluginsNames(): Seq[String]

  /** Lists names and short descriptions of plugins found on the system.
    */
  def pluginsHelp(): Seq[CommandHelp]
}
