import java.io.File

import sbt.{File, _}
import sbt.Keys._

import scala.sys.process._

object NativeImage {
  def buildNativeImage(staticOnLinux: Boolean): Def.Initialize[Task[Unit]] =
    Def
      .task {
        val log      = state.value.log
        val javaHome = System.getProperty("java.home")
        val nativeImagePath =
          if (isWindows)
            s"$javaHome\\bin\\native-image.cmd"
          else s"$javaHome/bin/native-image"
        val classPath =
          (Runtime / fullClasspath).value.files.mkString(File.pathSeparator)

        val additionalParameters =
          if (staticOnLinux && isLinux)
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

  def incrementalNativeImageBuild(
    actualBuild: TaskKey[Unit],
    artifactName: String
  ) =
    Def.taskDyn {
      def rebuild(reason: String) = {
        streams.value.log.info(
          s"$reason, forcing a rebuild."
        )
        Def.task {
          actualBuild.value
        }
      }

      val classpath = (Compile / fullClasspath).value
      val filesSet  = classpath.flatMap(f => f.data.allPaths.get()).toSet

      val store =
        streams.value.cacheStoreFactory.make("incremental_native_image")
      Tracked.diffInputs(store, FileInfo.hash)(filesSet) {
        sourcesDiff: ChangeReport[File] =>
          if (sourcesDiff.modified.nonEmpty)
            rebuild("Native Image is not up to date")
          else if (!artifactFile(artifactName).exists())
            rebuild("Native Image does not exist")
          else
            Def.task {
              streams.value.log.debug(
                "No source changes, Native Image is up to date."
              )
            }
      }
    }

  private def isWindows: Boolean =
    sys.props("os.name").toLowerCase().contains("windows")

  private def isLinux: Boolean =
    sys.props("os.name").toLowerCase().contains("linux")

  private def artifactFile(name: String): File =
    if (isWindows) file(name + ".exe")
    else file(name)
}
