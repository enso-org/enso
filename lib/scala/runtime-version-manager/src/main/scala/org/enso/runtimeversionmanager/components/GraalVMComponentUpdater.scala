package org.enso.runtimeversionmanager.components

import java.nio.file.Path

import org.enso.runtimeversionmanager.OS

import scala.sys.process._
import scala.util.{Success, Try}

/** Module that manages components of the GraalVM distribution.
  *
  * @param runtime the GraalVM runtime
  * @param os the operating system
  */
class GraalVMComponentUpdater(runtime: GraalRuntime, os: OS)
    extends RuntimeComponentUpdater {

  import GraalVMComponentUpdater._

  /** List the installed GraalVM components.
    *
    * @return the list of installed GraalVM components
    */
  override def list: Try[Seq[GraalVMComponent]] = {
    val suppressStderr = ProcessLogger(_ => ())
    val process = Process(
      Seq[String](Paths.gu, "list", "-v"),
      Some(runtime.path.toFile),
      ("JAVA_HOME", runtime.path),
      ("GRAALVM_HOME", runtime.path)
    )

    for {
      lines <- Try(process.lazyLines(suppressStderr))
    } yield ListOut.parse(lines)
  }

  /** Install the provided GraalVM components.
    *
    * @param components the list of components to install
    */
  override def install(components: Seq[GraalVMComponent]): Try[Unit] = {
    if (components.nonEmpty) {
      val componentsList = components.map(_.id)
      val process = Process(
        Seq[String](Paths.gu, "install") ++ componentsList,
        Some(runtime.path.toFile),
        ("JAVA_HOME", runtime.path),
        ("GRAALVM_HOME", runtime.path)
      )
      Try(process.!!)
    } else {
      Success(())
    }
  }

  private object Paths {

    /** Path to `gu` executable. */
    val gu: Path = os match {
      case OS.Linux   => runtime.path / "bin" / "gu"
      case OS.MacOS   => runtime.path / "bin" / "gu"
      case OS.Windows => runtime.path / "bin" / "gu.cmd"
    }
  }
}
object GraalVMComponentUpdater {

  implicit private def pathToString(path: Path): String =
    path.toAbsolutePath.toString

  implicit private class PathExtensions(path: Path) {

    def /(child: String): Path = path.resolve(child)
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
