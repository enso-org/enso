import sbt._

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
        runCompiler(project.path.toFile)
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

    val regularSuccess =
      withAggregate("Temporary_Aggregate_All", regular, runCompiler)
    // The name must contain `_Internal_` and `_Tests` to disable private check
    val internalSuccess =
      withAggregate("Temporary_Internal_Aggregate_Tests", internal, runCompiler)
    regularSuccess && internalSuccess
  }

  private def runCompiler(path: File): Boolean = {
    log.debug(s"Linting $path")
    DistributionPackage.runEnginePackage(
      engineDistributionRoot,
      Seq(
        "--compile",
        path.getAbsoluteFile.toString,
        "--enable-static-analysis",
        "-Werror"
      ),
      log
    )
  }

  private def withAggregate[R](
    name: String,
    projects: Seq[EnsoProjects.Project],
    action: File => R
  ): R = {
    val aggregateProjectPath = newProject(baseDirectory / "test", name)
    try {
      val imports = projects.map { project =>
        val namespace = project.namespace.getOrElse {
          throw new RuntimeException(
            s"Project ${project.name} does not have a namespace but was included for linting."
          )
        }
        s"import ${namespace}.${project.name}"
      }
      val code     = imports.mkString("\n")
      val codeFile = aggregateProjectPath / "src" / "Main.enso"
      IO.write(codeFile, code)
      action(aggregateProjectPath)
    } finally {
      IO.delete(aggregateProjectPath)
    }
  }

  private def newProject(parentPath: File, name: String): File = {
    val path = parentPath / name
    if (path.exists()) {
      log.warn(s"Deleting leftover temporary project $path.")
      IO.delete(path)
      if (path.exists()) {
        throw new RuntimeException(
          s"Failed to delete leftover temporary project $path. Please cleanup manually."
        )
      }
    }

    val result = DistributionPackage.runEnginePackage(
      engineDistributionRoot,
      Seq(
        "--new",
        path.getPath()
      ),
      log
    )

    if (!result) {
      throw new RuntimeException(
        s"Failed to create project $name in $parentPath"
      )
    }

    path
  }
}

object EnsoLint {
  sealed trait LintTarget
  object LintTarget {
    case class FindByName(name: String) extends LintTarget
    case object All                     extends LintTarget
  }
}
