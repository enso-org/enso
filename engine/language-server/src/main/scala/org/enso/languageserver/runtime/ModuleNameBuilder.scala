package org.enso.languageserver.runtime

import java.nio.file.Path

import scala.util.Try

object ModuleNameBuilder {

  /**
    * Build module name from the file path.
    *
    * @param projectName the project name
    * @param root the project root
    * @param file the file path of the module
    * @return the module name
    */
  def build(projectName: String, root: Path, file: Path): Option[String] = {
    getModuleSegments(root, file).map { modules =>
      toModule(projectName +: modules :+ getModuleName(file))
    }
  }

  private def getModuleSegments(
    root: Path,
    file: Path
  ): Option[Vector[String]] = {
    Try(root.relativize(file)).toOption
      .map { relativePath =>
        val b = Vector.newBuilder[String]
        1.until(relativePath.getNameCount - 1)
          .foreach(i => b += relativePath.getName(i).toString)
        b.result()
      }
  }

  private def getModuleName(path: Path): String = {
    val fileName = path.getFileName.toString
    fileName.substring(0, fileName.lastIndexOf('.'))
  }

  private def toModule(segments: Iterable[String]): String =
    segments.mkString(".")

}
