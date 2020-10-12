import java.io.File

import sbt._
import sbt.Keys._
import scala.sys.process._

object NativeImage {
  def buildNativeImage(staticOnLinux: Boolean): Def.Initialize[Task[Unit]] =
    Def
      .task {
        val log      = state.value.log
        val javaHome = System.getProperty("java.home")
        val nativeImagePath =
          if (sys.props("os.name").contains("Windows"))
            s"$javaHome\\bin\\native-image.cmd"
          else s"$javaHome/bin/native-image"
        val classPath =
          (Runtime / fullClasspath).value.files.mkString(File.pathSeparator)

        val additionalParameters =
          if (staticOnLinux && sys.props("os.name").contains("Linux"))
            "--static"
          else ""
        val resourcesGlobOpt = "-H:IncludeResources=.*Main.enso$"

        val cmd =
          s"$nativeImagePath $additionalParameters $resourcesGlobOpt " +
          s"--no-fallback --initialize-at-build-time" +
          s" -cp $classPath ${(Compile / mainClass).value.get} enso"

        if (!file(nativeImagePath).exists()) {
          log.error("Native Image component not found in the JVM distribution.")
          log.error(
            "You can install Native Image with `gu install native-image`."
          )
          throw new RuntimeException(
            "Native Image build failed, " +
            "because Native Image component was not found."
          )
        }

        if (cmd.! != 0) {
          log.error("Native Image build failed.")
          throw new RuntimeException("Native Image build failed")
        }

        log.info("Native Image build successful.")
      }
      .dependsOn(Compile / compile)
}
