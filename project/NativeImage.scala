import java.io.File
import java.nio.file.Path

import sbt.{Def, File, _}
import sbt.Keys._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object NativeImage {

  /**
    * Specifies whether the build executable should include debug symbols. Should
    * be set to false for production builds. May work only on Linux.
    */
  private val includeDebugInfo: Boolean = false

  /**
    * Creates a task that builds a native image for the current project.
    *
    * @param artifactName name of the artifact to create
    * @param staticOnLinux specifies whether to link statically (applies only
    *                      on Linux)
    */
  def buildNativeImage(
    artifactName: String,
    staticOnLinux: Boolean,
    additionalOptions: Seq[String] = Seq.empty
  ): Def.Initialize[Task[Unit]] =
    Def
      .task {
        val log            = state.value.log
        val javaHome       = System.getProperty("java.home")
        val subProjectRoot = baseDirectory.value
        val nativeImagePath =
          if (isWindows)
            s"$javaHome\\bin\\native-image.cmd"
          else s"$javaHome/bin/native-image"
        val classPath =
          (Runtime / fullClasspath).value.files.mkString(File.pathSeparator)

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

        val debugParameters =
          if (includeDebugInfo) "-H:GenerateDebugInfo=1" else ""

        val staticParameters =
          if (staticOnLinux && isLinux) {
            // Note [Static Build On Linux]
            val buildCache =
              subProjectRoot / "build-cache"
            val path = ensureMuslIsInstalled(buildCache, log)
            s"--static -H:UseMuslC=$path"
          } else ""

        val resourcesGlobOpt = "-H:IncludeResources=.*Main.enso$"

        val configLocation =
          subProjectRoot / "native-image-config"
        val configs =
          if (configLocation.exists()) {
            val path = configLocation.toPath.toAbsolutePath
            log.debug(s"Picking up Native Image configuration from `$path`.")
            s"-H:ConfigurationFileDirectories=$path"
          } else {
            log.debug(
              "No Native Image configuration found, proceeding without it."
            )
            ""
          }

        val cmd =
          s"$nativeImagePath $staticParameters $debugParameters " +
          s"$resourcesGlobOpt $configs " +
          s"--no-fallback --initialize-at-build-time " +
          s"${additionalOptions.mkString(" ")} " +
          s"-cp $classPath ${(Compile / mainClass).value.get} enso"

        log.debug(cmd)

        if (cmd.! != 0) {
          log.error("Native Image build failed.")
          throw new RuntimeException("Native Image build failed")
        }

        log.info("Native Image build successful.")
      }
      .dependsOn(Compile / compile)

  /**
    * Creates a task which watches for changes of any compiled files or
    * dependencies and triggers a rebuild if and only if there are any changes.
    *
    * @param actualBuild reference to the task doing the actual Native Image
    *                    build, usually one returned by [[buildNativeImage]]
    * @param artifactName name of the artifact that is expected to be created
    *                     by the native image build
    * @return
    */
  def incrementalNativeImageBuild(
    actualBuild: TaskKey[Unit],
    artifactName: String
  ): Def.Initialize[Task[Unit]] =
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

  private val muslBundleUrl =
    "https://github.com/gradinac/musl-bundle-example/releases/download/v1.0/musl.tar.gz"

  /**
    * Ensures that the `musl` bundle is installed.
    *
    * Checks for existence of its directory and if it does not exist, downloads
    * and extracts the bundle. `musl` is needed for static builds on Linux.
    *
    * @param buildCache build-cache directory for the current project
    * @param log a logger instance
    * @return path to the `musl` bundle that can be passed to the Native Image
    *         as a parameter
    */
  private def ensureMuslIsInstalled(
    buildCache: File,
    log: ManagedLogger
  ): Path = {
    val muslRoot       = buildCache / "musl-1.2.0"
    val bundleLocation = muslRoot / "bundle"
    if (!bundleLocation.exists()) {
      log.info(
        "`musl` is required for a static build, but it is not installed for " +
        "this subproject."
      )
      try {
        log.info("A `musl` bundle will be downloaded.")
        buildCache.mkdirs()
        val bundle = buildCache / "musl-bundle.tar.gz"

        val downloadExitCode = (url(muslBundleUrl) #> bundle).!
        if (downloadExitCode != 0) {
          log.error("Cannot download `musl` bundle.")
          throw new RuntimeException(s"Cannot download `$muslBundleUrl`.")
        }

        muslRoot.mkdirs()
        val tarExitCode = Seq(
          "tar",
          "xf",
          bundle.toPath.toAbsolutePath.toString,
          "-C",
          muslRoot.toPath.toAbsolutePath.toString
        ).!
        if (tarExitCode != 0) {
          log.error(
            "An error occurred when extracting the `musl` library bundle."
          )
          throw new RuntimeException(s"Cannot extract $bundle.")
        }

        log.info("Installed `musl`.")
      } catch {
        case e: Exception =>
          throw new RuntimeException(
            "`musl` installation failed. Cannot proceed with a static " +
            "Native Image build.",
            e
          )
      }

    }

    bundleLocation.toPath.toAbsolutePath
  }

}

/* Note [Static Build On Linux]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The default `glibc` contains a bug that would cause crashes when downloading
 * files form the internet, which is a crucial Launcher functionality. Instead,
 * `musl` is suggested by Graal as an alternative libc. The sbt task
 * automatically downloads a bundle containing all requirements for a static
 * build with `musl`.
 *
 * Currently, to use `musl`, the `-H:UseMuslC=/path/to/musl/bundle` option has
 * to be added to the build. In the future, a `--libc=musl` option may be
 * preferred instead, as described at
 * https://github.com/oracle/graal/blob/master/substratevm/STATIC-IMAGES.md
 * or even `musl` may be included by default. This task may thus need an update
 * when moving to a newer version of Graal.
 */
