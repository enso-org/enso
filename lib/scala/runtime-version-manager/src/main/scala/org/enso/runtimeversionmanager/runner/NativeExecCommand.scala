package org.enso.runtimeversionmanager.runner

import org.enso.cli.OS
import org.enso.distribution.{DistributionManager, Environment}
import org.enso.runtimeversionmanager.components.Engine

import org.apache.tika.config.TikaConfig
import org.apache.tika.Tika

import java.nio.file.Path

case class NativeExecCommand(executablePath: Path) extends ExecCommand {
  override def path: String = executablePath.toString

  override def cmdArguments(
    engine: Engine,
    jvmSettings: JVMSettings
  ): Seq[String] =
    Seq("-Dcom.oracle.graalvm.isaot=true")

  override def javaHome: Option[String] = None
}

object NativeExecCommand {
  def apply(version: String): Option[NativeExecCommand] = {
    val env      = new Environment() {}
    val dm       = new DistributionManager(env)
    val execName = OS.executableName("enso")
    val fullExecPath =
      dm.paths.engines.resolve(version).resolve("bin").resolve(execName)

    if (fullExecPath.toFile.exists() && isBinary(fullExecPath)) {
      Some(NativeExecCommand(fullExecPath))
    } else None
  }

  private def isBinary(path: Path): Boolean = {
    try {
      val config    = TikaConfig.getDefaultConfig()
      val tika      = new Tika(config)
      val mimeTypes = config.getMimeRepository
      val mime      = tika.detect(path);
      val tpe       = mimeTypes.forName(mime).getType.getType
      tpe != null && tpe == "application"
    } catch {
      case _: Throwable =>
        false
    }
  }
}
