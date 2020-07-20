package org.enso.launcher

import java.io.File
import java.nio.file.{Files, Path}

import sys.process._
import org.enso.launcher.cli.{
  CommandHelp,
  PluginBehaviour,
  PluginInterceptedFlow,
  PluginNotFound
}

import scala.collection.Factory
import scala.util.Try
import scala.jdk.StreamConverters._
import scala.util.control.NonFatal

/**
  * An abstraction of the system environment.
  */
trait SystemEnvironment {

  /**
    * Returns the system PATH, if available.
    */
  def getSystemPath: Option[String]

  /**
    * Returns a list of system-dependent plugin extensions.
    *
    * By default, on Unix plugins should have no extensions. On Windows, `.exe`
    * and `.bat` are supported.
    */
  def getPluginExtensions: Seq[String] =
    if (System.getProperty("os.name").toLowerCase.contains("windows"))
      Seq("bat", "exe")
    else Seq()

}

/**
  * Implements a [[cli.PluginManager]] using the provided [[SystemEnvironment]].
  */
class PluginManagerImplementation(environment: SystemEnvironment)
    extends cli.PluginManager {

  /**
    * Checks if the provided name represents a valid plugin and tries to run it.
    * @param name name of the plugin
    * @param args arguments that should be passed to it
    * @return
    */
  override def tryRunningPlugin(
    name: String,
    args: Seq[String]
  ): PluginBehaviour =
    if (hasPlugin(name)) {
      try {
        val exitCode = (Seq(pluginCommandForName(name)) ++ args).!
        System.exit(exitCode)
        PluginInterceptedFlow
      } catch {
        case NonFatal(_) =>
          System.err.println(
            s"A plugin `$name` was found on system PATH, " +
            s"but it could not be executed."
          )
          PluginNotFound
      }

    } else PluginNotFound

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

    for {
      directory <- paths if Files.isDirectory(directory)
      file      <- Files.list(directory).toScala(Factory.arrayFactory)
      if Files.isExecutable(file)
      pluginName  <- pluginNameForExecutable(file.getFileName.toString)
      description <- tryDescribingPlugin(pluginName)
    } yield CommandHelp(pluginName, description)
  }

  override def pluginsNames(): Seq[String] = pluginsHelp().map(_.name)

  private def hasPlugin(name: String): Boolean =
    tryDescribingPlugin(name).isDefined

  private def tryDescribingPlugin(name: String): Option[String] = {
    def canonicalizeDescription(description: String): String =
      description.replace("\n", " ").trim
    val noOpLogger = new ProcessLogger {
      override def out(s: => String): Unit = {}
      override def err(s: => String): Unit = {}
      override def buffer[T](f: => T): T = f
    }
    val command = Seq(pluginCommandForName(name), synopsisOption)
    Try(command.!!(noOpLogger)).toOption.map(canonicalizeDescription)
  }

  private def pluginCommandForName(name: String): String = pluginPrefix + name

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
