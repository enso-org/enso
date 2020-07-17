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

trait SystemEnvironment {
  def getSystemPath: Option[String]
  def getPluginExtensions: Seq[String] =
    if (System.getProperty("os.name").toLowerCase.contains("windows"))
      Seq("bat", "exe")
    else Seq()

}

class PluginManagerImplementation(environment: SystemEnvironment)
    extends cli.PluginManager {
  override def tryRunningPlugin(
    name: String,
    args: Seq[String]
  ): PluginBehaviour =
    if (hasPlugin(name)) {
      PluginInterceptedFlow(() => {
        val exitCode = (Seq(pluginCommandForName(name)) ++ args).!
        System.exit(exitCode)
      })
    } else PluginNotFound

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
      description <- describePlugin(pluginName)
    } yield CommandHelp(pluginName, description)
  }

  override def pluginsNames(): Seq[String] = pluginsHelp().map(_.name)

  val synopsisOption: String                   = "--synopsis"
  private def hasPlugin(name: String): Boolean = describePlugin(name).isDefined
  def describePlugin(name: String): Option[String] = {
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

  private val pluginPrefix                               = "enso-"
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

object LocalSystemEnvironment extends SystemEnvironment {
  override def getSystemPath: Option[String] = Option(System.getenv("PATH"))
}

object PluginManager extends PluginManagerImplementation(LocalSystemEnvironment)
