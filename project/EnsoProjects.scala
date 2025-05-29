import sbt.*
import sbt.nio.file.FileTreeView

import java.nio.file.Path

/** Helper for locating Enso projects stored in the source tree.
  *
  * The projects include standard libraries, test suites, benchmarks.
  */
object EnsoProjects {
  case class Project(namespace: Option[String], name: String, path: Path) {
    def usesPrivateAccess: Boolean =
      path.getParent != null && path.getParent.getFileName.toString == "test" && name
        .contains(
          "_Internal_"
        ) && name.endsWith("_Tests")
  }

  def ofPath(path: Path): Project =
    EnsoProjects.Project(
      namespace = None,
      name      = path.getFileName.toString,
      path      = path
    )

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
          val projectRoot           = path.getParent
          val projectNameLevel      = projectRoot.getParent
          val projectNamespaceLevel = projectNameLevel.getParent
          // We skip one more dir as the top-most directory is the version number
          Project(
            namespace = Some(projectNamespaceLevel.getFileName.toString),
            name      = projectNameLevel.getFileName.toString,
            path      = projectRoot
          )
        })

    def findTests(): Seq[Project] =
      FileTreeView.default
        .list(rootGlob / "test" / "*" / "package.yaml")
        .filter(_._2.isRegularFile)
        .map(_._1)
        .map(path => {
          val projectRoot = path.getParent
          Project(
            namespace = Some("enso_dev"),
            name      = projectRoot.getFileName.toString,
            path      = projectRoot
          )
        })
  }
}
