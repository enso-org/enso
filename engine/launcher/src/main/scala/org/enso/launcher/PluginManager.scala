package org.enso.launcher

import java.io.File
import java.nio.file.{Files, Path}

import sys.process._
import org.enso.cli.{
  CommandHelp,
  PluginBehaviour,
  PluginInterceptedFlow,
  PluginManager,
  PluginNotFound
}

import scala.collection.Factory
import scala.util.Try
import scala.jdk.StreamConverters._

/**
  * An abstraction of the system environment.
  */
trait SystemEnvironment {

  /**
    * Returns the system PATH, if available.
    */
  def getSystemPath: Option[String]

  /**
    * Checks if the application is being run on Windows.
    */
  def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("windows")

  /**
    * Returns a list of system-dependent plugin extensions.
    *
    * By default, on Unix plugins should have no extensions. On Windows, `.exe`
    * `.bat` and `.cmd` are supported.
    */
  def getPluginExtensions: Seq[String] =
    if (isWindows)
      Seq(".exe", ".bat", ".cmd")
    else Seq()

  /**
    * Returns a list of directories that can be ignored when traversing the
    * system PATH looking for plugins.
    *
    * These could be system directories that should not contain plguins anyway,
    * but traversing them would greatly slow down plugin discovery.
    */
  def getIgnoredPathDirectories: Seq[Path] =
    if (isWindows) Seq(Path.of("C:\\Windows")) else Seq()
}

/**
  * Implements a [[PluginManager]] using the provided [[SystemEnvironment]].
  */
class PluginManagerImplementation(environment: SystemEnvironment)
    extends PluginManager {

  /**
    * Checks if the provided name represents a valid plugin and tries to run it.
    *
    * @param name name of the plugin
    * @param args arguments that should be passed to it
    * @return
    */
  override def tryRunningPlugin(
    name: String,
    args: Seq[String]
  ): PluginBehaviour =
    findPlugin(name) match {
      case Some(PluginDescription(commandName, _)) =>
        val exitCode = (Seq(commandName) ++ args).!
        System.exit(exitCode)
        PluginInterceptedFlow
      case None =>
        PluginNotFound
    }

  private val pluginPrefix           = "enso-"
  private val synopsisOption: String = "--synopsis"

  /**
    * Traverses all directories in the system PATH, looking for executable files
    * which names start with `enso-`. A valid plugin must support a `synopsis`
    * option, i.e. running `enso-foo --synopsis` should return a short
    * description of the plugin and return with exit code 0 for the plugin to be
    * considered valid.
    */
  override def pluginsHelp(): Seq[CommandHelp] = {

    /**
      * Tries to parse a path string and returns Some(path) on success.
      *
      * We prefer silent failures here (returning None and skipping that entry),
      * as we don't want to fail the whole command if the PATH contains some
      * unparseable entries.
      */
    def safeParsePath(str: String): Option[Path] =
      Try(Path.of(str)).toOption

    val paths = environment.getSystemPath
      .map(_.split(File.pathSeparatorChar).toSeq.flatMap(safeParsePath))
      .getOrElse(Seq())

    def isIgnored(directory: Path): Boolean =
      environment.getIgnoredPathDirectories.exists(directory.startsWith)

    for {
      directory <- paths if Files.isDirectory(directory)
      if !isIgnored(directory)
      file <- Files.list(directory).toScala(Factory.arrayFactory)
      if Files.isExecutable(file)
      pluginName  <- pluginNameForExecutable(file.getFileName.toString)
      description <- findPlugin(pluginName)
    } yield CommandHelp(pluginName, description.synopsis)
  }

  override def pluginsNames(): Seq[String] = pluginsHelp().map(_.name)

  case class PluginDescription(executableName: String, synopsis: String)

  /**
    * Checks if the plugin with the given name is installed and valid. It tries
    * to execute it (checking various command extensions depending on the OS)
    * and check if it returns a synopsis.
    *
    * @param name name of the plugin
    * @return PluginDescription containing the command name that should be used
    *         to call the plugin and its synopsis
    */
  private def findPlugin(name: String): Option[PluginDescription] = {
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

  /**
    * Returns a sequence of possible commands a plugin with the given name may
    * be called by.
    */
  private def pluginCommandsForName(name: String): Seq[String] =
    Seq(pluginPrefix + name) ++
    environment.getPluginExtensions.map(ext => pluginPrefix + name + ext)

  private def pluginNameForExecutable(executableName: String): Option[String] =
    if (executableName.startsWith(pluginPrefix)) {
      Some(stripPlatformSuffix(executableName.stripPrefix(pluginPrefix)))
    } else None

  private def stripPlatformSuffix(executableName: String): String = {
    val extension =
      environment.getPluginExtensions.find(executableName.endsWith)
    extension match {
      case Some(extension) => executableName.stripSuffix(extension)
      case None            => executableName
    }
  }
}

/**
  * Implements the default [[SystemEnvironment]].
  */
object LocalSystemEnvironment extends SystemEnvironment {
  override def getSystemPath: Option[String] = Option(System.getenv("PATH"))
}

object PluginManager extends PluginManagerImplementation(LocalSystemEnvironment)
