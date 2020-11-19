package org.enso.runtimeversionmanager.runner

import java.nio.file.Path

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.Environment
import org.enso.runtimeversionmanager.components.Manifest.JVMOptionsContext
import org.enso.runtimeversionmanager.components.{
  Engine,
  GraalRuntime,
  RuntimeVersionManager
}
import org.enso.loggingservice.LogLevel

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, TimeoutException}
import scala.util.Try

/** A helper class that prepares settings for running Enso components and
  * converts these settings to actual commands that launch the component inside
  * of a JVM.
  */
class Runner(
  runtimeVersionManager: RuntimeVersionManager,
  environment: Environment,
  loggerConnection: Future[Option[Uri]]
) {

  /** The current working directory that is a starting point when checking if
    * the command is launched inside of a project.
    *
    * Can be overridden in tests.
    */
  protected val currentWorkingDirectory: Path = Path.of(".")

  def newProject(
    path: Path,
    name: String,
    version: SemVer,
    authorName: Option[String],
    authorEmail: Option[String],
    additionalArguments: Seq[String]
  ): Try[RunSettings] =
    Try {
      val authorNameOption =
        authorName.map(Seq("--new-project-author-name", _)).getOrElse(Seq())
      val authorEmailOption =
        authorEmail.map(Seq("--new-project-author-email", _)).getOrElse(Seq())
      val arguments =
        Seq(
          "--new",
          path.toAbsolutePath.normalize.toString,
          "--new-project-name",
          name
        ) ++ authorNameOption ++ authorEmailOption ++ additionalArguments
      RunSettings(version, arguments, connectLoggerIfAvailable = false)
    }

  /** Creates [[RunSettings]] for launching the Language Server. */
  def startLanguageServer(
    options: LanguageServerOptions,
    project: Project,
    versionOverride: Option[SemVer],
    logLevel: LogLevel,
    additionalArguments: Seq[String]
  ): Try[RunSettings] =
    Try {
      val version = versionOverride.getOrElse(project.version)
      val arguments = Seq(
        "--server",
        "--root-id",
        options.rootId.toString,
        "--path",
        project.path.toAbsolutePath.normalize.toString,
        "--interface",
        options.interface,
        "--rpc-port",
        options.rpcPort.toString,
        "--data-port",
        options.dataPort.toString,
        "--log-level",
        logLevel.toString
      )
      RunSettings(
        version,
        arguments ++ additionalArguments,
        // TODO [RW] set to true when language server gets logging support
        //  (#1144)
        connectLoggerIfAvailable = false
      )
    }

  final private val JVM_OPTIONS_ENV_VAR = "ENSO_JVM_OPTS"

  /** Runs an action giving it a command that can be used to launch the
    * component.
    *
    * While the action is executed, it is guaranteed that the component
    * referenced by the command is available. The command is considered invalid
    * after the `action` exits.
    *
    * Combines the [[RunSettings]] for the runner with the [[JVMSettings]] for
    * the underlying JVM to get the full command for launching the component.
    */
  def withCommand[R](runSettings: RunSettings, jvmSettings: JVMSettings)(
    action: Command => R
  ): R = {
    def prepareAndRunCommand(engine: Engine, javaCommand: JavaCommand): R = {
      val jvmOptsFromEnvironment = environment.getEnvVar(JVM_OPTIONS_ENV_VAR)
      jvmOptsFromEnvironment.foreach { opts =>
        Logger[Runner].debug(
          s"Picking up additional JVM options ($opts) from the " +
          s"$JVM_OPTIONS_ENV_VAR environment variable."
        )
      }

      val runnerJar = engine.runnerPath.toAbsolutePath.normalize.toString

      def translateJVMOption(option: (String, String)): String = {
        val name  = option._1
        val value = option._2
        s"-D$name=$value"
      }

      val context = JVMOptionsContext(enginePackagePath = engine.path)

      val manifestOptions =
        engine.defaultJVMOptions.filter(_.isRelevant).map(_.substitute(context))
      val environmentOptions =
        jvmOptsFromEnvironment.map(_.split(' ').toIndexedSeq).getOrElse(Seq())
      val commandLineOptions = jvmSettings.jvmOptions.map(translateJVMOption)

      val jvmArguments =
        manifestOptions ++ environmentOptions ++ commandLineOptions ++
        Seq("-jar", runnerJar)

      val loggingConnectionArguments =
        if (runSettings.connectLoggerIfAvailable)
          forceLoggerConnectionArguments()
        else Seq()

      val command = Seq(javaCommand.executableName) ++
        jvmArguments ++ loggingConnectionArguments ++ runSettings.runnerArguments

      val extraEnvironmentOverrides =
        javaCommand.javaHomeOverride.map("JAVA_HOME" -> _).toSeq

      action(Command(command, extraEnvironmentOverrides))
    }

    val engineVersion = runSettings.version
    if (jvmSettings.useSystemJVM) {
      runtimeVersionManager.withEngine(engineVersion) { engine =>
        prepareAndRunCommand(engine, systemJavaCommand)
      }
    } else {
      runtimeVersionManager.withEngineAndRuntime(engineVersion) {
        (engine, runtime) =>
          prepareAndRunCommand(engine, javaCommandForRuntime(runtime))
      }
    }
  }

  /** Represents a way of launching the JVM.
    *
    * Stores the name of the `java` executable to run and a possible JAVA_HOME
    * environment variable override.
    */
  private case class JavaCommand(
    executableName: String,
    javaHomeOverride: Option[String]
  )

  /** The [[JavaCommand]] representing the system-configured JVM.
    */
  private def systemJavaCommand: JavaCommand = JavaCommand("java", None)

  /** The [[JavaCommand]] representing a managed [[GraalRuntime]].
    */
  private def javaCommandForRuntime(runtime: GraalRuntime): JavaCommand =
    JavaCommand(
      executableName = runtime.javaExecutable.toAbsolutePath.normalize.toString,
      javaHomeOverride =
        Some(runtime.javaHome.toAbsolutePath.normalize.toString)
    )

  /** Returns arguments that should be added to a launched component to connect
    * it to launcher's logging service.
    *
    * It waits until the logging service has been set up and should be called as
    * late as possible (for example after installing any required components) to
    * avoid blocking other actions by the logging service setup.
    *
    * If the logging service is not available in 3 seconds after calling this
    * method, it assumes that it failed to boot and returns arguments that will
    * cause the launched component to use its own local logging.
    */
  private def forceLoggerConnectionArguments(): Seq[String] = {
    val connectionSetting = {
      try {
        Await.result(loggerConnection, 3.seconds)
      } catch {
        case exception: TimeoutException =>
          Logger[GraalRuntime].warn(
            "The logger has not been set up within the 3 second time limit, " +
            "the launched component will be started but it will not be " +
            "connected to the logging service.",
            exception
          )
          None
      }
    }
    connectionSetting match {
      case Some(uri) => Seq("--logger-connect", uri.toString)
      case None      => Seq()
    }
  }
}
