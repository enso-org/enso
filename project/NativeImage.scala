import java.io.File
import java.nio.file.Path

import sbt.{Def, File, _}
import sbt.Keys._
import sbt.internal.util.ManagedLogger
import sbtassembly.AssemblyKeys.assembly
import sbtassembly.AssemblyPlugin.autoImport.assemblyOutputPath

import scala.sys.process._

object NativeImage {

  /** Specifies whether the build executable should include debug symbols.
    * Should be set to false for production builds. May work only on Linux.
    */
  private val includeDebugInfo: Boolean = false

  /** Creates a task that builds a native image for the current project.
    *
    * This task must be setup in such a way that the assembly JAR is built
    * before it starts, as it uses this JAR for the build. Usually this can be
    * done by appending `.dependsOn(LocalProject("project-name") / assembly)`.
    *
    * Additional Native Image configuration can be set for each project by
    * editing configuration files in subdirectories of `META-INF/native-image`
    * of its resources directory. More information can be found at
    * [[https://github.com/oracle/graal/blob/master/substratevm/BuildConfiguration.md]].
    *
    * @param artifactName name of the artifact to create
    * @param staticOnLinux specifies whether to link statically (applies only
    *                      on Linux)
    * @param initializeAtBuildtime specifies if classes should be initialized at
    *                              build time by default
    * @param additionalOptions additional options for the Native Image build
    *                          tool
    * @param memoryLimitGigabytes a memory limit for the build tool, in
    *                             gigabytes; it is good to set this limit to
    *                             make GC more aggressive thus allowing it to
    *                             build successfully even with limited memory
    * @param initializeAtRuntime a list of classes that should be initialized at
    *                            run time - useful to set exceptions if build
    *                            time initialization is set to default
    */
  def buildNativeImage(
    artifactName: String,
    staticOnLinux: Boolean,
    initializeAtBuildtime: Boolean    = true,
    additionalOptions: Seq[String]    = Seq.empty,
    memoryLimitGigabytes: Option[Int] = Some(3),
    initializeAtRuntime: Seq[String]  = Seq.empty
  ): Def.Initialize[Task[Unit]] = Def
    .task {
      val log            = state.value.log
      val javaHome       = System.getProperty("java.home")
      val subProjectRoot = baseDirectory.value
      val nativeImagePath =
        if (Platform.isWindows)
          s"$javaHome\\bin\\native-image.cmd"
        else s"$javaHome/bin/native-image"
      val pathToJAR =
        (assembly / assemblyOutputPath).value.toPath.toAbsolutePath.normalize

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
        if (includeDebugInfo) Seq("-H:GenerateDebugInfo=1") else Seq()

      val (staticParameters, pathExts) =
        if (staticOnLinux && Platform.isLinux) {
          // Note [Static Build On Linux]
          val buildCache =
            subProjectRoot / "build-cache"
          val path = ensureMuslIsInstalled(buildCache, log)
          (Seq("--static", "--libc=musl"), Seq(path.toString))
        } else (Seq(), Seq())

      val configLocation =
        subProjectRoot / "native-image-config"
      val configs =
        if (configLocation.exists()) {
          val path = configLocation.toPath.toAbsolutePath
          log.debug(s"Picking up Native Image configuration from `$path`.")
          Seq(s"-H:ConfigurationFileDirectories=$path")
        } else {
          log.debug(
            "No Native Image configuration found, proceeding without it."
          )
          Seq()
        }

      val memoryLimitOptions =
        memoryLimitGigabytes.map(gigs => s"-J-Xmx${gigs}G").toSeq

      val initializeAtRuntimeOptions =
        if (initializeAtRuntime.isEmpty) Seq()
        else {
          val classes = initializeAtRuntime.mkString(",")
          Seq(s"--initialize-at-run-time=$classes")
        }

      val initializeAtBuildtimeOptions =
        if (initializeAtBuildtime) Seq("--initialize-at-build-time") else Seq()

      val cmd =
        Seq(nativeImagePath) ++
        debugParameters ++ staticParameters ++ configs ++
        Seq("--no-fallback", "--no-server") ++
        initializeAtBuildtimeOptions ++
        memoryLimitOptions ++ initializeAtRuntimeOptions ++
        additionalOptions ++
        Seq("-jar", pathToJAR.toString) ++
        Seq(artifactName)

      val pathParts = pathExts ++ Option(System.getenv("PATH")).toSeq
      val newPath   = pathParts.mkString(File.pathSeparator)

      log.debug(s"""PATH="$newPath" ${cmd.mkString(" ")}""")

      val process =
        Process(cmd, None, "PATH" -> newPath)

      if (process.! != 0) {
        log.error("Native Image build failed.")
        throw new RuntimeException("Native Image build failed")
      }

      log.info("Native Image build successful.")
    }
    .dependsOn(Compile / compile)

  /** Creates a task which watches for changes of any compiled files or
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
              streams.value.log.info(
                s"No source changes, $artifactName Native Image is up to date."
              )
            }
      }
    }

  /** [[File]] representing the artifact called `name` built with the Native
    * Image.
    */
  def artifactFile(name: String): File =
    if (Platform.isWindows) file(name + ".exe")
    else file(name)

  private val muslBundleUrl =
    "https://github.com/gradinac/musl-bundle-example/releases/download/" +
    "v1.0/musl.tar.gz"

  /** Ensures that the `musl` bundle is installed.
    *
    * Checks for existence of its directory and if it does not exist, downloads
    * and extracts the bundle. After extracting it does the required
    * initialization (renaming paths to be absolute and creating a shell script
    * called `musl-gcc`).
    *
    * `musl` is needed for static builds on Linux.
    *
    * @param buildCache build-cache directory for the current project
    * @param log a logger instance
    * @return path to the `musl` bundle binary directory which should be added
    *         to PATH of the launched native-image
    */
  private def ensureMuslIsInstalled(
    buildCache: File,
    log: ManagedLogger
  ): Path = {
    val muslRoot       = buildCache / "musl-1.2.0"
    val bundleLocation = muslRoot / "bundle"
    val binaryLocation = bundleLocation / "bin"
    val gccLocation    = binaryLocation / "musl-gcc"
    def isMuslInstalled =
      gccLocation.exists() && gccLocation.isOwnerExecutable
    if (!isMuslInstalled) {
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

        replacePathsInSpecs(
          bundleLocation / "lib" / "musl-gcc.specs",
          bundleLocation
        )
        createGCCWrapper(bundleLocation)

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

    binaryLocation.toPath.toAbsolutePath.normalize
  }

  /** Replaces paths in `musl-gcc.specs` with absolute paths to the bundle.
    *
    * The paths in `musl-gcc.specs` start with `/build/bundle` which is not a
    * valid path by default. Instead, these prefixes are replaced with an
    * absolute path to the bundle.
    *
    * @param specs reference to `musl-gcc.specs` file
    * @param bundleLocation location of the bundle root
    */
  private def replacePathsInSpecs(specs: File, bundleLocation: File): Unit = {
    val content    = IO.read(specs)
    val bundlePath = bundleLocation.toPath.toAbsolutePath.normalize.toString
    val replaced   = content.replace("/build/bundle", bundlePath)
    IO.write(specs, replaced)
  }

  /** Creates a simple shell script called `musl-gcc` which calls the original
    * `gcc` and ensures the bundle's configuration (`musl-gcc.specs`) is loaded.
    */
  private def createGCCWrapper(bundleLocation: File): Unit = {
    val bundlePath = bundleLocation.toPath.toAbsolutePath.normalize.toString
    val content =
      s"""#!/bin/sh
         |exec "$${REALGCC:-gcc}" "$$@" -specs "$bundlePath/lib/musl-gcc.specs"
         |""".stripMargin
    val wrapper = bundleLocation / "bin" / "musl-gcc"
    IO.write(wrapper, content)
    wrapper.setExecutable(true)
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
 * The `musl` bundle that we use is not guaranteed to be maintained, so if in
 * the future a new version of `musl` comes out and we need to upgrade, we may
 * need to create our own infrastructure (a repository with CI jobs for creating
 * such bundles). It is especially important to note that the libstdc++ that is
 * included in this bundle should also be built using `musl` as otherwise linker
 * errors may arise.
 *
 * Currently, to use `musl`, the `--libc=musl` option has to be added to the
 * build and `gcc-musl` must be available in the system PATH for the
 * native-image. In the future it is possible that a different option will be
 * used or that the bundle will not be required anymore if it became
 * prepackaged. This task may thus need an update when moving to a newer version
 * of Graal.
 *
 * Currently to make the bundle work correctly with GraalVM 20.2, a shell script
 * called `gcc-musl` which loads the bundle's configuration is created by the
 * task and the paths starting with `/build/bundle` in `musl-gcc.specs` are
 * replaced with absolute paths to the bundle location.
 */
