package org.enso.build.stdlibupdater

import java.nio.file.Path
import scala.util.control.NonFatal

object StdlibVersionUpdater {
  def main(args: Array[String]): Unit = try {
    val visitor = args match {
      case Array("check")  => ReportingVisitor
      case Array("update") => UpdatingVisitor
      case _ =>
        throw new IllegalArgumentException(
          "The program accepts exactly one argument, which can be either " +
          "`check` or `update`."
        )
    }
    val bundledLibRoot        = Path.of("distribution/lib")
    val targetVersion: String = buildinfo.Info.stdLibVersion

    val walker = new StdlibWalker(bundledLibRoot, targetVersion, visitor)
    walker.walk()
  } catch {
    case NonFatal(error) =>
      println(s"Failed: $error")
      error.printStackTrace()
      sys.exit(1)
  }
}
