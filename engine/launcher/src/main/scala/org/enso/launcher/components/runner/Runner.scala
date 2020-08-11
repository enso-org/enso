package org.enso.launcher.components.runner

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.{GlobalConfigurationManager, Logger}
import org.enso.launcher.components.{ComponentsManager, Runtime}
import org.enso.launcher.project.ProjectManager

class Runner(
  projectManager: ProjectManager,
  configurationManager: GlobalConfigurationManager,
  componentsManager: ComponentsManager
) {
  def repl(
    projectPath: Option[Path],
    versionOverride: Option[SemVer],
    additionalArguments: Seq[String]
  ): RunSettings = {
    val inProject = projectPath match {
      case Some(value) =>
        Some(projectManager.loadProject(value).get)
      case None =>
        projectManager.findProject(Path.of("."))
    }

    val version =
      versionOverride.getOrElse {
        inProject.map(_.version).getOrElse(configurationManager.defaultVersion)
      }
    val arguments = inProject match {
      case Some(project) =>
        val projectPackagePath = project.path.toAbsolutePath.normalize.toString
        Seq("--repl", "--in-project", projectPackagePath)
      case None =>
        Seq("--repl")
    }
    RunSettings(version, arguments ++ additionalArguments)
  }

  def run(
    path: Option[Path],
    versionOverride: Option[SemVer],
    additionalArguments: Seq[String]
  ): RunSettings = {
    val actualPath = path
      .getOrElse {
        projectManager
          .findProject(Path.of("."))
          .getOrElse {
            Logger.error(
              "The current directory is not inside any project. `enso run` " +
              "should either get a path to a project or script to run, or be " +
              "run inside of a project to run that project."
            )
            sys.exit(1)
          }
          .path
      }
      .toAbsolutePath
      .normalize()
    if (!Files.exists(actualPath)) {
      Logger.error(s"$actualPath does not exist")
      sys.exit(1)
    }
    val projectMode = Files.isDirectory(actualPath)
    val project =
      if (projectMode) Some(projectManager.loadProject(actualPath).get)
      else projectManager.findProject(actualPath)
    val version = versionOverride
      .orElse(project.map(_.version))
      .getOrElse(configurationManager.defaultVersion)

    val arguments =
      if (projectMode) Seq("--run", actualPath.toString)
      else
        project match {
          case Some(project) =>
            Seq(
              "--run",
              actualPath.toString,
              "--in-project",
              project.path.toAbsolutePath.normalize().toString
            )
          case None =>
            Seq("--run", actualPath.toString)
        }
    RunSettings(version, arguments ++ additionalArguments)
  }

  def languageServer(
    options: LanguageServerOptions,
    versionOverride: Option[SemVer],
    additionalArguments: Seq[String]
  ): RunSettings = {
    val project = projectManager.loadProject(options.path).get
    val version = versionOverride.getOrElse(project.version)
    val arguments = Seq(
      "--server",
      "--root-id",
      options.rootId.toString,
      "--path",
      options.path.toAbsolutePath.normalize.toString,
      "--interface",
      options.interface,
      "--rpc-port",
      options.rpcPort.toString,
      "--data-port",
      options.dataPort.toString
    )
    RunSettings(version, arguments ++ additionalArguments)
  }

  def createCommand(
    runSettings: RunSettings,
    jvmSettings: JVMSettings
  ): Command = {
    val engine = componentsManager.findOrInstallEngine(runSettings.version)
    val javaCommand =
      if (jvmSettings.useSystemJVM) systemJavaCommand
      else {
        val runtime = componentsManager.findOrInstallRuntime(engine)
        javaCommandForRuntime(runtime)
      }

    val runtimeJar = engine.runtimePath.toAbsolutePath.normalize.toString
    val runnerJar  = engine.runnerPath.toAbsolutePath.normalize.toString
    val allJvmOptions =
      Seq(("truffle.class.path.append", runtimeJar)) ++
      engine.defaultJVMOptions ++ jvmSettings.jvmOptions

    def translateJVMOption(option: (String, String)): String = {
      val name  = option._1
      val value = option._2
      s"-D$name=$value"
    }

    val jvmArguments =
      allJvmOptions.map(translateJVMOption) ++ Seq("-jar", runnerJar)

    val command = Seq(javaCommand.executableName) ++
      jvmArguments ++ runSettings.runnerArguments

    val extraEnvironmentOverrides =
      javaCommand.javaHomeOverride.map("JAVA_HOME" -> _).toSeq

    Command(command, extraEnvironmentOverrides)
  }

  private case class JavaCommand(
    executableName: String,
    javaHomeOverride: Option[String]
  )

  private def systemJavaCommand: JavaCommand = JavaCommand("java", None)
  private def javaCommandForRuntime(runtime: Runtime): JavaCommand =
    JavaCommand(
      executableName = runtime.javaExecutable.toAbsolutePath.normalize.toString,
      javaHomeOverride =
        Some(runtime.javaHome.toAbsolutePath.normalize.toString)
    )
}
