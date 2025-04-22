import sbt.*
import sbt.nio.file.FileTreeView

import java.nio.file.Path

/** Helper for locating Enso projects stored in the source tree.
  *
  * The projects include standard libraries, test suites, benchmarks.
  */
object EnsoProjects {
  case class Project(name: String, path: Path)

  class ProjectFinder(
    val root: Path
  ) {

    private val rootGlob = root.toAbsolutePath.toGlob

    def findStandardLibraries(): Seq[Project] =
      FileTreeView.default
        .list(
          rootGlob / "distribution" / "lib" / "*" / "*" / "*" / "package.yaml"
        )
        .filter(_._2.isRegularFile)
        .map(_._1)
        .map(path => {
          val projectRoot = path.getParent
          // We skip one more dir as the top-most directory is the version number
          Project(projectRoot.getParent.getFileName.toString, projectRoot)
        })

    def findTests(): Seq[Project] =
      FileTreeView.default
        .list(rootGlob / "test" / "*" / "package.yaml")
        .filter(_._2.isRegularFile)
        .map(_._1)
        .map(path => {
          val projectRoot = path.getParent
          Project(projectRoot.getFileName.toString, projectRoot)
        })
  }
}
