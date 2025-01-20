package org.enso.runtimeversionmanager.runner

import org.enso.runtimeversionmanager.components.{Engine, GraalRuntime}
import org.enso.runtimeversionmanager.components.Manifest.JVMOptionsContext

/** Represents a way of launching the JVM.
  *
  * @param executableName name of the `java` executable to run
  * @param javaHomeOverride if set, asks to override the JAVA_HOME environment
  *                         variable when launching the JVM
  */
class JavaExecCommand(
  val executableName: String,
  val javaHome: Option[String]
) extends ExecCommand {
  def path: String = executableName

  override def cmdArguments(
    engine: Engine,
    jvmSettings: JVMSettings
  ): Seq[String] = {
    def translateJVMOption(
      option: (String, String),
      standardOption: Boolean
    ): String = {
      val name  = option._1
      val value = option._2
      if (standardOption) s"-D$name=$value" else s"--$name=$value"
    }

    val context = JVMOptionsContext(enginePackagePath = engine.path)

    val manifestOptions =
      engine.defaultJVMOptions
        .filter(_.isRelevant)
        .map(_.substitute(context))
    val commandLineOptions = jvmSettings.jvmOptions.map(
      translateJVMOption(_, standardOption = true)
    ) ++ jvmSettings.extraOptions.map(
      translateJVMOption(_, standardOption = false)
    )
    val componentPath = engine.componentDirPath.toAbsolutePath.normalize
    val modulePathOptions =
      Seq(
        "--module-path",
        componentPath.toString,
        "-m",
        "org.enso.runner/org.enso.runner.Main"
      )

    manifestOptions ++ commandLineOptions ++ modulePathOptions
  }
}

object JavaExecCommand {

  /** The [[JavaExecCommand]] representing the system-configured JVM.
    */
  def defaultSystem: JavaExecCommand = new JavaExecCommand("java", None)

  /** The [[JavaExecCommand]] representing a managed [[GraalRuntime]].
    */
  def forRuntime(runtime: GraalRuntime): JavaExecCommand =
    new JavaExecCommand(
      executableName = runtime.javaExecutable.toAbsolutePath.normalize.toString,
      javaHome       = Some(runtime.javaHome.toAbsolutePath.normalize.toString)
    )

}
