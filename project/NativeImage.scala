import java.io.File
import java.nio.file.Path

import sbt._
import sbt.Keys._
import sbt.internal.util.ManagedLogger
import sbtassembly.AssemblyKeys.assembly
import sbtassembly.AssemblyPlugin.autoImport.assemblyOutputPath

import scala.sys.process._
import java.nio.file.Paths

object NativeImage {

  /** Specifies whether the build executable should include debug symbols.
    * Should be set to false for production builds. May work only on Linux.
    */
  private val includeDebugInfo: Boolean = false

  lazy val smallJdk = taskKey[Option[File]]("Location of a minimal JDK")
  lazy val additionalCp =
    taskKey[Seq[String]](
      "Additional class-path entries to be added to the native image"
    )

  /** List of classes that should be initialized at build time by the native image.
    * Note that we strive to initialize as much classes during the native image build
    * time as possible, as this reduces the time needed to start the native image.
    * One wildcard could theoretically be used instead of the list, but to make things
    * more explicit, we use the list.
    */
  private val defaultBuildTimeInitClasses = Seq(
    "org",
    "org.enso",
    "scala",
    "java",
    "sun",
    "cats",
    "io",
    "shapeless",
    "com",
    "izumi",
    "zio",
    "enumeratum",
    "akka",
    "nl",
    "ch.qos.logback"
  )

  val NATIVE_IMAGE_ARG_FILE = "native-image-args.txt"

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
    * @param additionalOptions additional options for the Native Image build
    *                          tool
    * @param buildMemoryLimitMegabytes a memory limit for the build tool, in
    *                             megabytes; it is good to set this limit to
    *                             make GC more aggressive thus allowing it to
    *                             build successfully even with limited memory
    * @param runtimeThreadStackMegabytes the runtime thread stack size; the
    *                             minimum for ZIO to work is higher than the
    *                             default value on some systems
    * @param initializeAtRuntime a list of classes that should be initialized at
    *                            run time - useful to set exceptions if build
    *                            time initialization is set to default
    * @param initializeAtBuildtime a list of classes that should be initialized at
    *                              build time.
    * @param includeRuntime Whether `org.enso.runtime` should is included. If yes, then
    *                       it will be passed as a module to the native-image along with other
    *                       Graal and Truffle related modules.
    * @param verbose whether to print verbose output from the native image.
    */
  def buildNativeImage(
    name: String,
    staticOnLinux: Boolean,
    targetDir: File                          = null,
    additionalOptions: Seq[String]           = Seq.empty,
    buildMemoryLimitMegabytes: Option[Int]   = Some(15608),
    runtimeThreadStackMegabytes: Option[Int] = Some(2),
    initializeAtRuntime: Seq[String]         = Seq.empty,
    initializeAtBuildtime: Seq[String]       = defaultBuildTimeInitClasses,
    mainClass: Option[String]                = None,
    verbose: Boolean                         = false,
    includeRuntime: Boolean                  = true
  ): Def.Initialize[Task[Unit]] = Def
    .task {
      val log       = state.value.log
      val targetLoc = artifactFile(targetDir, name, false)

      def nativeImagePath(prefix: Path)(path: Path): Path = {
        val base = path.resolve(prefix)
        if (Platform.isWindows)
          base.resolve("native-image.cmd")
        else base.resolve("native-image")
      }

      val (javaHome: Path, nativeImagePathResolver) =
        smallJdk.value
          .map(f =>
            (f.toPath(), nativeImagePath(Path.of("lib", "svm", "bin")) _)
          )
          .filter { case (p, resolver) => resolver(p).toFile.exists() }
          .getOrElse(
            (
              Paths.get(System.getProperty("java.home")),
              nativeImagePath(Path.of("bin")) _
            )
          )

      log.info("Native image JAVA_HOME: " + javaHome)

      val subProjectRoot = baseDirectory.value

      if (!nativeImagePathResolver(javaHome).toFile.exists()) {
        log.error(
          "Unexpected: Native Image component not found in the JVM distribution: " + javaHome
        )
        log.error("Is this a GraalVM distribution?")
        log.error(
          "On older distributions, you can install Native Image with `gu install native-image`."
        )
        throw new RuntimeException(
          "Native Image build failed, " +
          "because native-image binary was not found."
        )
      }
      if (additionalOptions.contains("--language:java")) {
        log.warn(
          s"Building ${targetLoc} image with experimental Espresso support!"
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

      val quickBuildOption =
        if (BuildInfo.isReleaseMode) Seq() else Seq("-Ob")

      val buildMemoryLimitOptions =
        buildMemoryLimitMegabytes.map(megs => s"-J-Xmx${megs}M").toSeq

      val runtimeMemoryOptions =
        runtimeThreadStackMegabytes.map(megs => s"-R:StackSize=${megs}M").toSeq

      val initializeAtBuildtimeOptions =
        if (initializeAtBuildtime.isEmpty) Seq()
        else {
          val classes = initializeAtBuildtime.mkString(",")
          Seq(s"--initialize-at-build-time=$classes")
        }

      val initializeAtRuntimeOptions =
        if (initializeAtRuntime.isEmpty) Seq()
        else {
          val classes = initializeAtRuntime.mkString(",")
          Seq(s"--initialize-at-run-time=$classes")
        }

      val runtimeCp = (LocalProject("runtime") / Runtime / fullClasspath).value
      val runnerCp =
        (LocalProject("engine-runner") / Runtime / fullClasspath).value
      val ourCp      = (Runtime / fullClasspath).value
      val cpToSearch = (ourCp ++ runtimeCp ++ runnerCp).distinct
      val componentModules: Seq[String] = JPMSUtils
        .filterModulesFromClasspath(
          cpToSearch,
          JPMSUtils.componentModules,
          log,
          projName = (moduleName.value),
          scalaBinaryVersion.value,
          shouldContainAll = true
        )
        .map(_.data.getAbsolutePath)

      val auxCp = additionalCp.value
      val fullCp =
        if (includeRuntime) {
          componentModules ++ auxCp
        } else {
          ourCp.map(_.data.getAbsolutePath) ++ auxCp
        }
      val cpStr = fullCp.mkString(File.pathSeparator)
      log.debug("Class-path: " + cpStr)

      val verboseOpt = if (verbose) Seq("--verbose") else Seq()

      var args: Seq[String] =
        Seq("-cp", cpStr) ++
        quickBuildOption ++
        debugParameters ++ staticParameters ++ configs ++
        Seq("--no-fallback", "--no-server") ++
        Seq("-march=compatibility") ++
        initializeAtBuildtimeOptions ++
        initializeAtRuntimeOptions ++
        buildMemoryLimitOptions ++
        runtimeMemoryOptions ++
        additionalOptions ++
        Seq("-o", targetLoc.toString)

      args = mainClass match {
        case Some(main) =>
          args ++
          Seq(main)
        case None =>
          val pathToJAR =
            (assembly / assemblyOutputPath).value.toPath.toAbsolutePath.normalize
          args ++
          Seq("-jar", pathToJAR.toString)
      }

      val targetDirValue = (Compile / target).value
      val argFile        = targetDirValue.toPath.resolve(NATIVE_IMAGE_ARG_FILE)
      IO.writeLines(argFile.toFile, args, append = false)

      val pathParts = pathExts ++ Option(System.getenv("PATH")).toSeq
      val newPath   = pathParts.mkString(File.pathSeparator)

      val cmd =
        Seq(nativeImagePathResolver(javaHome).toString) ++
        verboseOpt ++
        Seq("@" + argFile.toAbsolutePath.toString)

      log.debug(s"""PATH="$newPath" ${cmd.mkString(" ")}""")

      val process =
        Process(cmd, None, "PATH" -> newPath)

      // All the output from native-image is redirected into a StringBuilder, and printed
      // at the end of the build. This mitigates the problem when there are multiple sbt
      // commands running in parallel and the output is intertwined.
      val sb = new StringBuilder
      val processLogger = ProcessLogger(str => {
        log.info(str)
        sb.append(str + System.lineSeparator())
      })
      log.info(
        s"Started building $targetLoc native image. The output is captured."
      )
      val retCode    = process.!(processLogger)
      val targetFile = artifactFile(targetDir, name, true)
      if (retCode != 0 || !targetFile.exists()) {
        log.error(s"Native Image build of $targetFile failed, with output: ")
        println(sb.toString())
        throw new RuntimeException("Native Image build failed")
      }
      log.info(s"$targetLoc native image build successful.")
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
    name: String,
    targetDir: File = null
  ): Def.Initialize[Task[Unit]] =
    Def.taskDyn {
      def rebuild(reason: String) = {
        streams.value.log.info(
          s"$reason, forcing a rebuild."
        )
        val artifact = artifactFile(targetDir, name)
        if (artifact.exists()) {
          artifact.delete()
        }
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
          else if (!artifactFile(targetDir, name).exists())
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
  def artifactFile(
    targetDir: File,
    name: String,
    withExtension: Boolean = false
  ): File = {
    val artifactName =
      if (withExtension && Platform.isWindows) name + ".exe"
      else name
    if (targetDir == null) {
      new File(artifactName).getAbsoluteFile()
    } else {
      new File(targetDir, artifactName)
    }
  }

  private val muslBundleUrl =
    "https://github.com/gradinac/musl-bundle-example/releases/download/" +
    "v1.0/musl.tar.gz"

  /** Ensures that the `musl` bundle is installed.
    *
    * Checks for existence of its directory and if it does not exist, downloads
    * and extracts the bundle. After extracting it does the required
    * initialization (renaming paths to be absolute and creating a shell script
    * called `x86_64-linux-musl-gcc`).
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
    val gccLocation    = binaryLocation / "x86_64-linux-musl-gcc"
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
    val wrapper = bundleLocation / "bin" / "x86_64-linux-musl-gcc"
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
 * build and `x86_64-linux-musl-gcc` must be available in the system PATH for the
 * native-image. In the future it is possible that a different option will be
 * used or that the bundle will not be required anymore if it became
 * prepackaged. This task may thus need an update when moving to a newer version
 * of Graal.
 *
 * Currently to make the bundle work correctly with GraalVM 20.2, a shell script
 * called `x86_64-linux-musl-gcc` which loads the bundle's configuration is created by the
 * task and the paths starting with `/build/bundle` in `musl-gcc.specs` are
 * replaced with absolute paths to the bundle location.
 */
