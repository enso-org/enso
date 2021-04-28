package org.enso.runtimeversionmanager.components

import java.nio.file.Path

import com.typesafe.scalalogging.Logger

import scala.sys.process._
import scala.util.{Success, Try}

/** Module that manages components of the GraalVM distribution.
  *
  * @param runtime the GraalVM runtime
  */
class GraalVMComponentUpdater(runtime: GraalRuntime)
    extends RuntimeComponentUpdater {

  import GraalVMComponentUpdater._

  private val logger = Logger[GraalVMComponentUpdater]
  private val gu     = runtime.findExecutable("gu")

  /** List the installed GraalVM components.
    *
    * @return the list of installed GraalVM components
    */
  override def list: Try[Seq[GraalVMComponent]] = {
    val command: Seq[String] = Seq(gu, "list", "-v")
    val process = Process(
      command,
      Some(runtime.javaHome.toFile),
      ("JAVA_HOME", runtime.javaHome),
      ("GRAALVM_HOME", runtime.javaHome)
    )

    logger.trace(
      s"command=${command.mkString(" ")}; " +
      s"JAVA_HOME=${Properties(runtime.javaHome)}" +
      s"gu=${Properties(gu)}"
    )
    for {
      stdout <- Try(process.lazyLines(stderrLogger))
      _ = logger.trace(stdout.mkString(System.lineSeparator()))
    } yield ListOut.parse(stdout)
  }

  /** Install the provided GraalVM components.
    *
    * @param components the list of components to install
    */
  override def install(components: Seq[GraalVMComponent]): Try[Unit] = {
    if (components.nonEmpty) {
      val command: Seq[String] =
        Seq[String](gu, "install") ++ components.map(_.id)
      val process = Process(
        command,
        Some(runtime.path.toFile),
        ("JAVA_HOME", runtime.javaHome),
        ("GRAALVM_HOME", runtime.javaHome)
      )
      logger.trace(
        s"command=${command.mkString(" ")}; " +
        s"JAVA_HOME=${Properties(runtime.path)}" +
        s"gu=${Properties(gu)}"
      )
      for {
        stdout <- Try(process.lazyLines(stderrLogger))
        _ = logger.trace(stdout.mkString(System.lineSeparator()))
      } yield ()
    } else {
      Success(())
    }
  }

  private def stderrLogger =
    ProcessLogger(err => logger.trace(s"[stderr] $err"))
}
object GraalVMComponentUpdater {

  implicit private def pathToString(path: Path): String =
    path.toAbsolutePath.toString

  /** Debug file properties. */
  private case class Properties(path: Path) {

    private val file = path.toFile

    override def toString: String =
      s"${path.toAbsolutePath} { " +
      s"exists=${file.exists()}; " +
      s"executable=${file.canExecute}; " +
      "}"
  }

  /** Parser for the `gu list -v` command output. */
  object ListOut {

    private val ID: String      = "ID"
    private val separator: Char = ':'

    /** Extract the GraalVM components from the gu output.
      *
      * @param lines the gu output
      * @return the list of GraalVM components.
      */
    def parse(lines: Seq[String]): Seq[GraalVMComponent] =
      lines
        .filter(_.startsWith(ID))
        .map(_.dropWhile(_ != separator).drop(1).trim)
        .map(GraalVMComponent(_))
  }
}
