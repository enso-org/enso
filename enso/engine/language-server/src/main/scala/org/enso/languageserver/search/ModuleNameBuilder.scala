package org.enso.languageserver.search

import java.nio.file.Path

import scala.util.Try

object ModuleNameBuilder {

  /** Build the module name from the file path.
    *
    * @param projectName the project name
    * @param root the project root directory
    * @param file the module file path
    * @return the module name
    */
  def build(projectName: String, root: Path, file: Path): Option[String] = {
    getModuleSegments(root, file).map { modules =>
      toModule(projectName +: modules :+ getModuleName(file))
    }
  }

  /** Extract segments related to the module from the file path.
    *
    * @param root the project root directory
    * @param file the module file path
    * @return the list of module segments
    */
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

  /** Get the module name from the file path.
    *
    * @param file the module file path
    * @return the module name
    */
  private def getModuleName(file: Path): String = {
    val fileName = file.getFileName.toString
    fileName.substring(0, fileName.lastIndexOf('.'))
  }

  /** Convert the list of segments to a module name.
    *
    * @param segments the list of segments
    * @return the fully qualified module name
    */
  private def toModule(segments: Iterable[String]): String =
    segments.mkString(".")

}
