package org.enso.launcher.components.runner

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.GlobalConfigurationManager
import org.enso.launcher.components.{ComponentsManager, Runtime}
import org.enso.launcher.project.ProjectManager

import scala.util.Try

/**
  * A helper class that prepares settings for running Enso components and
  * converts these settings to actual commands that launch the component inside
  * of a JVM.
  */
class Runner(
  projectManager: ProjectManager,
  configurationManager: GlobalConfigurationManager,
  componentsManager: ComponentsManager
) {

  /**
    * The current working directory that is a starting point when checking if
    * the command is launched inside of a project.
    *
    * Can be overridden in tests.
    */
  protected val currentWorkingDirectory: Path = Path.of(".")

  /**
    * Creates [[RunSettings]] for launching the REPL.
    *
    * See [[org.enso.launcher.Launcher.runRepl]] for more details.
    */
  def repl(
    projectPath: Option[Path],
    versionOverride: Option[SemVer],
    additionalArguments: Seq[String]
  ): Try[RunSettings] =
    Try {
      val inProject = projectPath match {
        case Some(value) =>
          Some(projectManager.loadProject(value).get)
        case None =>
          projectManager.findProject(currentWorkingDirectory).get
      }

      val version =
        versionOverride.getOrElse {
          inProject
            .map(_.version)
            .getOrElse(configurationManager.defaultVersion)
        }
      val arguments = inProject match {
        case Some(project) =>
          val projectPackagePath =
            project.path.toAbsolutePath.normalize.toString
          Seq("--repl", "--in-project", projectPackagePath)
        case None =>
          Seq("--repl")
      }
      RunSettings(version, arguments ++ additionalArguments)
    }

  /**
    * Creates [[RunSettings]] for running Enso projects or scripts.
    *
    * See [[org.enso.launcher.Launcher.runRun]] for more details.
    */
  def run(
    path: Option[Path],
    versionOverride: Option[SemVer],
    additionalArguments: Seq[String]
  ): Try[RunSettings] =
    Try {
      val actualPath = path
        .getOrElse {
          projectManager
            .findProject(currentWorkingDirectory)
            .get
            .getOrElse {
              throw RunnerError(
                "The current directory is not inside any project. `enso run` " +
                "should either get a path to a project or script to run, or " +
                "be run inside of a project to run that project."
              )
            }
            .path
        }
        .toAbsolutePath
        .normalize()
      if (!Files.exists(actualPath)) {
        throw RunnerError(s"$actualPath does not exist")
      }
      val projectMode = Files.isDirectory(actualPath)
      val project =
        if (projectMode) Some(projectManager.loadProject(actualPath).get)
        else projectManager.findProject(actualPath).get
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

  /**
    * Creates [[RunSettings]] for launching the Language Server.
    *
    * See [[org.enso.launcher.Launcher.runLanguageServer]] for more details.
    */
  def languageServer(
    options: LanguageServerOptions,
    versionOverride: Option[SemVer],
    additionalArguments: Seq[String]
  ): Try[RunSettings] =
    Try {
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

  /**
    * Creates a command that can be used to launch the component.
    *
    * Combines the [[RunSettings]] for the runner with the [[JVMSettings]] for
    * the underlying JVM to get the full command for launching the component.
    */
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

  /**
    * Represents a way of launching the JVM.
    *
    * Stores the name of the `java` executable to run and a possible JAVA_HOME
    * environment variable override.
    */
  private case class JavaCommand(
    executableName: String,
    javaHomeOverride: Option[String]
  )

  /**
    * The [[JavaCommand]] representing the system-configured JVM.
    */
  private def systemJavaCommand: JavaCommand = JavaCommand("java", None)

  /**
    * The [[JavaCommand]] representing a managed [[Runtime]].
    */
  private def javaCommandForRuntime(runtime: Runtime): JavaCommand =
    JavaCommand(
      executableName = runtime.javaExecutable.toAbsolutePath.normalize.toString,
      javaHomeOverride =
        Some(runtime.javaHome.toAbsolutePath.normalize.toString)
    )
}
