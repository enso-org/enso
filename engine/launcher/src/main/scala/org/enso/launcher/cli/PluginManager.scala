package org.enso.launcher.cli

import java.nio.file.{Files, Path}
import org.enso.cli.arguments
import org.enso.cli.arguments.CommandHelp
import org.enso.distribution.{Environment, FileSystem}
import org.enso.launcher.distribution.LauncherEnvironment

import scala.sys.process._
import scala.util.Try

/** Implements an [[arguments.PluginManager]] using the given
  * [[Environment]].
  */
class PluginManager(env: Environment) extends arguments.PluginManager {

  /** Checks if the provided name represents a valid plugin and tries to run it.
    *
    * @param name name of the plugin
    * @param args arguments that should be passed to it
    * @return exit code of the launched plugin
    */
  override def runPlugin(
    name: String,
    args: Seq[String]
  ): Int =
    findPlugin(name) match {
      case Some(PluginDescription(commandName, _)) =>
        (Seq(commandName) ++ args).!
      case None =>
        throw new RuntimeException(
          "Internal error: Could not find the plugin. " +
          "This should not happen if hasPlugin returned true earlier."
        )
    }

  /** @inheritdoc
    */
  override def hasPlugin(name: String): Boolean = findPlugin(name).isDefined

  private val pluginPrefix           = "enso-"
  private val synopsisOption: String = "--synopsis"

  /** Traverses all directories in the system PATH, looking for executable files
    * which names start with `enso-`. A valid plugin must support a `synopsis`
    * option, i.e. running `enso-foo --synopsis` should return a short
    * description of the plugin and return with exit code 0 for the plugin to be
    * considered valid.
    */
  override def pluginsHelp(): Seq[CommandHelp] = {
    def isIgnored(directory: Path): Boolean =
      env.getIgnoredPathDirectories.exists(directory.startsWith)

    for {
      directory <- env.getSystemPath if Files.isDirectory(directory)
      if !isIgnored(directory)
      file <- FileSystem.listDirectory(directory)
      if Files.isExecutable(file)
      pluginName  <- pluginNameForExecutable(file.getFileName.toString)
      description <- findPlugin(pluginName)
    } yield CommandHelp(pluginName, description.synopsis)
  }

  /** @inheritdoc
    */
  override def pluginsNames(): Seq[String] = pluginsHelp().map(_.name)

  /** A short description of a plugin consisting of its command name and
    * synopsis.
    */
  case class PluginDescription(executableName: String, synopsis: String)

  private val pluginsCache
    : collection.mutable.HashMap[String, Option[PluginDescription]] =
    collection.mutable.HashMap.empty

  /** Checks if the plugin with the given name is installed and valid.
    *
    * It tries to execute it (checking various command extensions depending on
    * the OS) and check if it returns a synopsis.
    *
    * Results of this function are cached to avoid executing the plugin's
    * `--synopsis` multiple times.
    *
    * @param name name of the plugin
    * @return [[PluginDescription]] containing the command name that should be
    *        used to call the plugin and its synopsis
    */
  private def findPlugin(name: String): Option[PluginDescription] =
    pluginsCache.getOrElseUpdate(name, lookupPlugin(name))

  private def lookupPlugin(name: String): Option[PluginDescription] = {
    def canonicalizeDescription(description: String): String =
      description.replace("\n", " ").trim
    val noOpLogger = new ProcessLogger {
      override def out(s: => String): Unit = {}
      override def err(s: => String): Unit = {}
      override def buffer[T](f: => T): T = f
    }

    def getSynopsis(commandName: String): Option[String] = {
      val command = Seq(commandName, synopsisOption)
      Try(command.!!(noOpLogger)).toOption.map(canonicalizeDescription)
    }

    for (commandName <- pluginCommandsForName(name)) {
      val synopsis = getSynopsis(commandName)
      synopsis match {
        case Some(value) => return Some(PluginDescription(commandName, value))
        case None        =>
      }
    }

    None
  }

  /** Returns a sequence of possible commands a plugin with the given name may
    * be called by.
    */
  private def pluginCommandsForName(name: String): Seq[String] =
    Seq(pluginPrefix + name) ++
    env.getPluginExtensions.map(ext => pluginPrefix + name + ext)

  private def pluginNameForExecutable(executableName: String): Option[String] =
    if (executableName.startsWith(pluginPrefix)) {
      Some(stripPlatformSuffix(executableName.stripPrefix(pluginPrefix)))
    } else None

  private def stripPlatformSuffix(executableName: String): String = {
    val extension =
      env.getPluginExtensions.find(executableName.endsWith)
    extension match {
      case Some(extension) => executableName.stripSuffix(extension)
      case None            => executableName
    }
  }
}

/** Default implementation of the [[PluginManager]] using the default
  * [[LauncherEnvironment]].
  */
object PluginManager extends PluginManager(LauncherEnvironment)
