package src.main.scala.licenses

import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import scala.collection.JavaConverters._

object FilesHelper {

  /** A helper method that recursively traverses the directory structure at
    * `root` and collects results of calling `action` on each encountered entry.
    *
    * The action is called for all kinds of entries that are encountered.
    */
  def walk[R](root: Path)(action: Path => Seq[R]): Seq[R] = {
    val stream = Files.walk(root)
    try {
      val list = stream.collect(Collectors.toList())
      list.asScala.flatMap(action)
    } finally {
      stream.close()
    }
  }
}
