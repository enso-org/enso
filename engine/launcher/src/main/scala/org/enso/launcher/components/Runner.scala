package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.Logger

import scala.sys.process.Process

class Runner(
  componentsManager: ComponentsManager
) {
  def run(
    version: SemVer,
    useSystemJVM: Boolean,
    jvmOptions: Seq[(String, String)],
    runnerArguments: Seq[String]
  ): Int = {
    val engine = componentsManager.findOrInstallEngine(version)
    val javaCommand =
      if (useSystemJVM) systemJavaCommand
      else {
        val runtime = componentsManager.findOrInstallRuntime(engine)
        javaCommandForRuntime(runtime)
      }

    val runtimeJar = engine.runtimePath.toAbsolutePath.normalize.toString
    val runnerJar  = engine.runnerPath.toAbsolutePath.normalize.toString
    val allJvmOptions =
      Seq(
        ("truffle.class.path.append", runtimeJar)
      ) ++ engine.defaultJVMOptions ++ jvmOptions

    def translateJVMOption(option: (String, String)): String = {
      val name  = option._1
      val value = option._2
      s"-D$name=$value"
    }

    val jvmArguments =
      allJvmOptions.map(translateJVMOption) ++ Seq("-jar", runnerJar)

    val command =
      Seq(javaCommand.executableName) ++ jvmArguments ++ runnerArguments

    val extraEnvironmentOverrides =
      javaCommand.javaHomeOverride.map("JAVA_HOME" -> _).toSeq

    def environmentDescription =
      extraEnvironmentOverrides.map(v => s"${v._1}=${v._2} ").mkString
    Logger.debug(s"Executing $environmentDescription${command.mkString(" ")}")
    val process = Process(command, None, extraEnvironmentOverrides: _*)
      .run(connectInput = true)
    process.exitValue()
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
