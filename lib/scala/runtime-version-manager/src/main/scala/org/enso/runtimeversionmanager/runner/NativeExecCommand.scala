package org.enso.runtimeversionmanager.runner

import org.enso.cli.OS
import org.enso.distribution.{DistributionManager, Environment}
import org.enso.runtimeversionmanager.components.Engine
import org.apache.tika.config.TikaConfig
import org.apache.tika.Tika
import org.slf4j.Logger
import java.nio.file.Path

case class NativeExecCommand(executablePath: Path) extends ExecCommand {
  override def path: String = executablePath.toString

  override def cmdArguments(
    engine: Engine,
    jvmSettings: JVMSettings
  ): Seq[String] =
    Seq()

  override def javaHome: Option[String] = None
}

object NativeExecCommand {
  def apply(
    version: String,
    engine: Engine,
    logger: Logger
  ): Option[NativeExecCommand] = {
    val env      = new Environment() {}
    val dm       = new DistributionManager(env)
    val execName = OS.executableName("enso")
    val fullExecPath =
      dm.paths.engines.resolve(version).resolve("bin").resolve(execName)

    if (fullExecPath.toFile.exists() && isBinary(fullExecPath, logger)) {
      Some(NativeExecCommand(fullExecPath))
    } else {
      val component =
        engine.componentDirPath.resolve("..").toAbsolutePath.normalize
      val fallbackExecPath = component.resolve("bin").resolve(execName)
      if (
        fallbackExecPath.toFile.exists() && isBinary(fallbackExecPath, logger)
      ) {
        Some(NativeExecCommand(fallbackExecPath))
      } else {
        logger.warn(
          "Failed to find native launcher at a pre-determined location: {}",
          fallbackExecPath
        )
        None
      }
    }
  }

  private def isBinary(path: Path, logger: Logger): Boolean = {
    try {
      val config    = TikaConfig.getDefaultConfig()
      val tika      = new Tika(config)
      val mimeTypes = config.getMimeRepository
      val mime      = tika.detect(path);
      val tpe       = mimeTypes.forName(mime).getType.getType
      tpe != null && tpe == "application"
    } catch {
      case e: Throwable =>
        logger.warn("Failed to infer mimetype for path {}", path, e)
        false
    }
  }
}
