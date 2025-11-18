import EnsoProjects.ProjectFinder
import sbt.*

import java.nio.file.Path

/** Helper for running lint steps on Enso code. */
class EnsoLint(
  baseDirectory: File,
  engineDistributionRoot: File,
  log: sbt.Logger
) {
  def check(what: EnsoLint.LintTarget): Unit = {
    val projectFinder = new EnsoProjects.ProjectFinder(baseDirectory.toPath)
    val allProjects =
      projectFinder.findStandardLibraries() ++ projectFinder.findTests()

    val success = what match {
      case EnsoLint.LintTarget.FindByName(name) =>
        val foundByName = allProjects.filter(_.name == name)
        val project = foundByName match {
          case Seq(proj) => proj
          case _         => EnsoProjects.ofPath(Path.of(name))
        }
        runCompiler(Seq(project.path.toFile))
      case EnsoLint.LintTarget.All =>
        runAll(allProjects)
    }

    if (!success) {
      throw new RuntimeException(
        s"Linting failed due to warnings/errors."
      )
    }
  }

  private def runAll(projects: Seq[EnsoProjects.Project]): Boolean = {
    val (internal, regular) = projects.partition(_.usesPrivateAccess)

    val regularSuccess = runCompiler(regular.map(_.path.toFile))
    val internalSuccess = runCompiler(
      internal.map(_.path.toFile),
      disablePrivateCheck = true
    )
    regularSuccess && internalSuccess
  }

  private def runCompiler(
    paths: Seq[File],
    disablePrivateCheck: Boolean = false
  ): Boolean = {
    val pathNames = paths
      .map(p => nameSuffix(p.toPath))
      .sorted
      .mkString(", ")
    log.info(s"Linting [$pathNames]")

    val absPaths = paths
      .map(_.getAbsoluteFile.toString)
    val disablePrivateCheckArg = if (disablePrivateCheck) {
      Seq("--disable-private-check")
    } else {
      Seq()
    }
    val args = disablePrivateCheckArg ++ Seq(
      "--enable-static-analysis",
      "-Werror",
      "--compile"
    ) ++ absPaths

    DistributionPackage.runEnginePackage(
      engineDistributionRoot,
      args,
      log,
      Some(paths.head.getAbsoluteFile.getParentFile)
    )
  }

  private def nameSuffix(path: Path): String = {
    val suffix = 3
    if (path.getNameCount > suffix) {
      path.subpath(path.getNameCount - suffix, path.getNameCount).toString
    } else {
      path.toString
    }
  }
}

object EnsoLint {
  sealed trait LintTarget
  object LintTarget {
    case class FindByName(name: String) extends LintTarget
    case object All                     extends LintTarget
  }
}
