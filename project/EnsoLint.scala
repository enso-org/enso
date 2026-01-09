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
        runAll(projectFinder)
    }

    if (!success) {
      throw new RuntimeException(
        s"Linting failed due to warnings/errors."
      )
    }
  }

  /** Will run linting on all the projects - stdlibs and tests.
    * linting of stdlibs and tests cannot be run together as they have different
    * parent directory.
    * See https://github.com/enso-org/enso/pull/14296#discussion_r2538984048
    * @return true if all linting passed without errors/warnings
    */
  private def runAll(projectFinder: ProjectFinder): Boolean = {
    val stdLibs = projectFinder.findStandardLibraries()
    val (internal, regular) = projectFinder
      .findTests()
      .partition(_.usesPrivateAccess)

    val stdLibsSuccess = runCompiler(
      stdLibs.map(_.path.toFile)
    )
    val regularSuccess = runCompiler(regular.map(_.path.toFile))
    val internalSuccess = runCompiler(
      internal.map(_.path.toFile),
      disablePrivateCheck = true
    )
    stdLibsSuccess && regularSuccess && internalSuccess
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
    val env = Map(
      "JAVA_TOOL_OPTIONS" ->
      ("-Dorg.enso.compiler.noSourceArchives=" + absPaths.mkString(","))
    )
    val args = disablePrivateCheckArg ++ Seq(
      "--enable-static-analysis",
      "--hide-progress",
      "-Werror",
      "--compile"
    ) ++ absPaths

    DistributionPackage.runEnginePackage(
      engineDistributionRoot,
      args,
      log,
      Some(paths.head.getAbsoluteFile.getParentFile),
      env = env
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
