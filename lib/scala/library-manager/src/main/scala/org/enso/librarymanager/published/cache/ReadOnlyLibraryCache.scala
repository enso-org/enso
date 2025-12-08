package org.enso.librarymanager.published.cache

import org.enso.semver.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.resolved.LibraryRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.Using

/** A read-only cache may contain some pre-defined set of libraries, but it does
  * not necessarily install any additional libraries.
  *
  * An example of a read-only cache are libraries bundled with the engine.
  */
trait ReadOnlyLibraryCache {

  /** Locates the library in the cache and returns the path to its root if it
    * has been found.
    */
  def findCachedLibrary(
    libraryName: LibraryName,
    version: SemVer
  ): Option[LibraryRoot]
}

object ReadOnlyLibraryCache {
  def recursivelyToString(
                        path: Path,
                        indentPrefix: Int = 0,
                        depth: Int = 0,
                        maxDepth: Int = 5,
                        sb: StringBuilder = new StringBuilder): String = {
    val prefix = "  " * indentPrefix
    if (Files.isDirectory(path)) {
      val name = path.getFileName.toString + "/"
      sb.append(prefix + name + "\n")
      Using(Files.list(path)) { children =>
        children.forEach { child =>
          recursivelyToString(
            child,
            indentPrefix = indentPrefix + 1,
            depth = depth + 1,
            maxDepth = maxDepth,
            sb = sb
          )
        }
      }
    } else {
      val name = path.getFileName.toString
      if (name.endsWith(".yaml")) {
        println(prefix + name + ":")
        val lines = Files.readAllLines(path)
        val indentedLines = lines.stream.map(line => prefix + "  " + line).toList
        val content = indentedLines.asScala.mkString("\n")
        sb.append(content)
        sb.append("\n")
      } else {
        sb.append(prefix + name + "\n")
      }
    }
    sb.toString
  }
}
