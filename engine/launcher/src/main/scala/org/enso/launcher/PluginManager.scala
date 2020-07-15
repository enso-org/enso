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

    //println(paths)
    def monadPrint[F[_]](str: String)(unit: F[Unit]): F[Unit] = {
      //println(str)
      val _ = str
      unit
    }
    def sunit = Seq(())
    def ounit = Some(())

    for {
      directory <- paths if Files.isDirectory(directory)
      _         <- monadPrint(directory.toString)(sunit)
      file      <- Files.list(directory).toScala(Factory.arrayFactory)
      _         <- monadPrint(file.toString)(sunit)
      if Files.isExecutable(file)
      _           <- monadPrint(file.getFileName.toString)(sunit)
      pluginName  <- pluginNameForExecutable(file.getFileName.toString)
      _           <- monadPrint(pluginName)(ounit)
      description <- describePlugin(pluginName)
      _           <- monadPrint(file.getFileName.toString)(ounit)
    } yield CommandHelp(pluginName, description)
  }

  override def pluginsNames(): Seq[String] = pluginsHelp().map(_.name)

  private def hasPlugin(name: String): Boolean = describePlugin(name).isDefined

  def describePlugin(name: String): Option[String] =
    Try(Seq(pluginCommandForName(name), "--describe").!!).toOption

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
