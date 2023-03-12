import io.circe.yaml
import io.circe.syntax._
import sbt.internal.util.ManagedLogger
import sbt._
import sbt.io.syntax.fileToRichFile
import sbt.util.{CacheStore, CacheStoreFactory, FileInfo, Tracked}

import scala.sys.process._

object DistributionPackage {

  /** File extensions. */
  implicit class FileExtensions(file: File) {

    /** Get the outermost directory of this file. For absolute paths this
      * function always returns root.
      *
      * == Example ==
      * Get top directory of the relative path.
      * {{{
      *   file("foo/bar/baz").getTopDirectory == file("foo")
      * }}}
      *
      * Get top directory of the absolute path.
      * {{{
      *   file(/foo/bar/baz").getTopDirectory == file("/")
      * }}}
      *
      * @return the outermost directory of this file.
      */
    def getTopDirectory: File = {
      @scala.annotation.tailrec
      def go(path: File): File = {
        val parent = path.getParentFile
        if (parent == null) path else go(parent)
      }
      go(file)
    }
  }

  /** Conditional copying, based on the contents of cache and timestamps of files.
    *
    * @param source source directory
    * @param destination target directory
    * @param cache cache used for persisting the cached information
    * @return true, if copying was necessary, false if no change was detected between the directories
    */
  def copyDirectoryIncremental(
    source: File,
    destination: File,
    cache: CacheStore
  ): Boolean = {
    val allFiles = source.allPaths.get().toSet
    Tracked.diffInputs(cache, FileInfo.lastModified)(allFiles) { diff =>
      val missing = diff.unmodified.exists { f =>
        val relativePath = f.relativeTo(source).get
        val destinationFile =
          destination.toPath.resolve(relativePath.toPath).toFile
        !destinationFile.exists()
      }

      if (diff.modified.nonEmpty || diff.removed.nonEmpty || missing) {
        IO.delete(destination)
        IO.copyDirectory(source, destination)
        true
      } else false
    }
  }

  def copyFilesIncremental(
    sources: Seq[File],
    destinationDirectory: File,
    cache: CacheStore
  ): Unit = {
    val allFiles = sources.toSet
    IO.createDirectory(destinationDirectory)
    Tracked.diffInputs(cache, FileInfo.lastModified)(allFiles) { diff =>
      for (f <- diff.removed) {
        IO.delete(destinationDirectory / f.getName)
      }
      for (f <- diff.modified -- diff.removed) {
        IO.copyFile(f, destinationDirectory / f.getName)
      }
      for (f <- diff.unmodified) {
        val destinationFile = destinationDirectory / f.getName
        if (!destinationFile.exists()) {
          IO.copyFile(f, destinationDirectory / f.getName)
        }
      }
    }
  }

  def executableName(baseName: String): String =
    if (Platform.isWindows) baseName + ".exe" else baseName

  def createProjectManagerPackage(
    distributionRoot: File,
    cacheFactory: CacheStoreFactory
  ): Unit = {
    copyDirectoryIncremental(
      file("distribution/project-manager/THIRD-PARTY"),
      distributionRoot / "THIRD-PARTY",
      cacheFactory.make("project-manager-third-party")
    )

    copyFilesIncremental(
      Seq(file(executableName("project-manager"))),
      distributionRoot / "bin",
      cacheFactory.make("project-manager-exe")
    )
  }

  def createEnginePackage(
    distributionRoot: File,
    cacheFactory: CacheStoreFactory,
    log: Logger,
    graalVersion: String,
    javaVersion: String,
    ensoVersion: String,
    editionName: String,
    sourceStdlibVersion: String,
    targetStdlibVersion: String,
    targetDir: File
  ): Unit = {
    copyDirectoryIncremental(
      file("distribution/engine/THIRD-PARTY"),
      distributionRoot / "THIRD-PARTY",
      cacheFactory.make("engine-third-party")
    )

    copyFilesIncremental(
      Seq(file("runtime.jar"), file("runner.jar")),
      distributionRoot / "component",
      cacheFactory.make("engine-jars")
    )
    val parser = targetDir / Platform.dynamicLibraryFileName("enso_parser")
    copyFilesIncremental(
      Seq(parser),
      distributionRoot / "component",
      cacheFactory.make("engine-parser-library")
    )

    (distributionRoot / "editions").mkdirs()
    Editions.writeEditionConfig(
      editionsRoot   = distributionRoot / "editions",
      ensoVersion    = ensoVersion,
      editionName    = editionName,
      libraryVersion = targetStdlibVersion,
      log            = log
    )

    copyLibraryCacheIncremental(
      sourceRoot      = file("distribution/lib"),
      destinationRoot = distributionRoot / "lib",
      sourceVersion   = sourceStdlibVersion,
      targetVersion   = targetStdlibVersion,
      cacheFactory    = cacheFactory.sub("engine-libraries"),
      log             = log
    )

    copyDirectoryIncremental(
      file("distribution/bin"),
      distributionRoot / "bin",
      cacheFactory.make("engine-bin")
    )

    buildEngineManifest(
      template     = file("distribution/manifest.template.yaml"),
      destination  = distributionRoot / "manifest.yaml",
      graalVersion = graalVersion,
      javaVersion  = javaVersion
    )

    indexStdLib(
      stdLibVersion  = targetStdlibVersion,
      ensoVersion    = ensoVersion,
      stdLibRoot     = distributionRoot / "lib",
      ensoExecutable = distributionRoot / "bin" / "enso",
      cacheFactory   = cacheFactory.sub("stdlib"),
      log            = log
    )
  }

  def indexStdLib(
    stdLibVersion: String,
    ensoVersion: String,
    stdLibRoot: File,
    ensoExecutable: File,
    cacheFactory: CacheStoreFactory,
    log: Logger
  ): Unit = {
    for {
      libMajor <- stdLibRoot.listFiles()
      libName  <- (stdLibRoot / libMajor.getName).listFiles()
    } yield {
      val cache = cacheFactory.make(s"$libName.$ensoVersion")
      val path  = libName / ensoVersion
      Tracked.diffInputs(cache, FileInfo.lastModified)(
        path.globRecursive("*.enso").get().toSet
      ) { diff =>
        if (diff.modified.nonEmpty) {
          println(s"Generating index for ${libName} ")
          val command = Seq(
            Platform.executableFileName(ensoExecutable.toString),
            "--no-compile-dependencies",
            "--compile",
            path.toString
          )
          log.debug(command.mkString(" "))
          val exitCode = Process(
            command,
            None,
            "JAVA_OPTS" -> "-Dorg.jline.terminal.dumb=true"
          ).!
          if (exitCode != 0) {
            throw new RuntimeException(s"Cannot compile $libMajor.$libName.")
          }
        } else {
          println(s"No modified files. Not generating index for ${libName} ")
        }
      }
    }
  }

  def runEnginePackage(
    distributionRoot: File,
    args: Seq[String],
    log: Logger
  ): Boolean = {
    import scala.collection.JavaConverters._

    val enso = distributionRoot / "bin" / "enso"
    log.info(s"Executing $enso ${args.mkString(" ")}")
    val pb  = new java.lang.ProcessBuilder()
    val all = new java.util.ArrayList[String]()
    all.add(enso.getAbsolutePath())
    all.addAll(args.asJava)
    pb.command(all)
    pb.inheritIO()
    val p        = pb.start()
    val exitCode = p.waitFor()
    if (exitCode != 0) {
      log.warn(enso + " finished with exit code " + exitCode)
    }
    exitCode == 0
  }

  def fixLibraryManifest(
    packageRoot: File,
    targetVersion: String,
    log: Logger
  ): Unit = {
    val packageConfig   = packageRoot / "package.yaml"
    val originalContent = IO.read(packageConfig)
    yaml.parser.parse(originalContent) match {
      case Left(error) =>
        log.error(s"Failed to parse $packageConfig: $error")
        throw error
      case Right(parsed) =>
        val obj = parsed.asObject.getOrElse {
          throw new IllegalStateException(s"Incorrect format of $packageConfig")
        }

        val key        = "version"
        val updated    = obj.remove(key).add(key, targetVersion.asJson)
        val serialized = yaml.printer.print(updated.asJson)
        if (serialized == originalContent) {
          log.info(
            s"No need to update $packageConfig, already in correct version."
          )
        } else {
          IO.write(packageConfig, serialized)
          log.debug(s"Updated $packageConfig to $targetVersion")
        }
    }
  }

  def copyLibraryCacheIncremental(
    sourceRoot: File,
    destinationRoot: File,
    sourceVersion: String,
    targetVersion: String,
    cacheFactory: CacheStoreFactory,
    log: Logger
  ): Unit = {
    val existingLibraries =
      collection.mutable.ArrayBuffer.empty[(String, String)]
    for (prefix <- sourceRoot.list()) {
      for (libName <- (sourceRoot / prefix).list()) {
        val targetPackageRoot =
          destinationRoot / prefix / libName / targetVersion
        copyDirectoryIncremental(
          source      = sourceRoot / prefix / libName / sourceVersion,
          destination = targetPackageRoot,
          cache       = cacheFactory.make(s"$prefix.$libName")
        )
        fixLibraryManifest(targetPackageRoot, targetVersion, log)
        existingLibraries.append((prefix, libName))
      }
    }

    val existingLibrariesSet = existingLibraries.toSet
    for (prefix <- destinationRoot.list()) {
      for (libName <- (destinationRoot / prefix).list()) {
        if (!existingLibrariesSet.contains((prefix, libName))) {
          log.info(
            s"Removing a library $prefix.$libName from the distribution, " +
            s"because it does not exist in the sources anymore."
          )
        }
      }
    }

  }

  private def buildEngineManifest(
    template: File,
    destination: File,
    graalVersion: String,
    javaVersion: String
  ): Unit = {
    val base = IO.read(template)
    val extensions =
      s"""graal-vm-version: $graalVersion
         |graal-java-version: $javaVersion
         |""".stripMargin
    IO.write(destination, base + extensions)
  }

  def createLauncherPackage(
    distributionRoot: File,
    cacheFactory: CacheStoreFactory
  ): Unit = {
    copyDirectoryIncremental(
      file("distribution/launcher/THIRD-PARTY"),
      distributionRoot / "THIRD-PARTY",
      cacheFactory.make("launcher-third-party")
    )

    copyFilesIncremental(
      Seq(file(executableName("enso"))),
      distributionRoot / "bin",
      cacheFactory.make("launcher-exe")
    )

    IO.createDirectory(distributionRoot / "dist")
    IO.createDirectory(distributionRoot / "runtime")

    copyFilesIncremental(
      Seq(
        file("distribution/launcher/.enso.portable"),
        file("distribution/launcher/README.md")
      ),
      distributionRoot,
      cacheFactory.make("launcher-rootfiles")
    )
  }

  sealed trait OS {
    def name:                String
    def hasSupportForSulong: Boolean
    def graalName: String                    = name
    def executableName(base: String): String = base
    def archiveExt: String                   = ".tar.gz"
    def isUNIX: Boolean                      = true
  }
  object OS {
    case object Linux extends OS {
      override val name: String                 = "linux"
      override val hasSupportForSulong: Boolean = true
    }
    case object MacOS extends OS {
      override val name: String                 = "macos"
      override val hasSupportForSulong: Boolean = true
      override def graalName: String            = "darwin"
    }
    case object Windows extends OS {
      override val name: String                         = "windows"
      override val hasSupportForSulong: Boolean         = false
      override def executableName(base: String): String = base + ".exe"
      override def archiveExt: String                   = ".zip"
      override def isUNIX: Boolean                      = false
    }

    val platforms = Seq(Linux, MacOS, Windows)

    def apply(name: String): Option[OS] =
      name.toLowerCase match {
        case Linux.`name`   => Some(Linux)
        case MacOS.`name`   => Some(MacOS)
        case Windows.`name` => Some(Windows)
        case _              => None
      }
  }

  sealed trait Architecture {
    def name: String
  }
  object Architecture {
    case object X64 extends Architecture {
      override def name: String = "amd64"
    }

    val archs = Seq(X64)
  }

  /** A helper class that manages building distribution artifacts. */
  class Builder(
    ensoVersion: String,
    graalVersion: String,
    graalJavaVersion: String,
    artifactRoot: File
  ) {

    def artifactName(
      component: String,
      os: OS,
      architecture: Architecture
    ): String =
      s"enso-$component-$ensoVersion-${os.name}-${architecture.name}"

    def graalInPackageName: String =
      s"graalvm-ce-java$graalJavaVersion-$graalVersion"

    private def extractZip(archive: File, root: File): Unit = {
      IO.createDirectory(root)
      val exitCode = Process(
        Seq("unzip", "-q", archive.toPath.toAbsolutePath.normalize.toString),
        cwd = Some(root)
      ).!
      if (exitCode != 0) {
        throw new RuntimeException(s"Cannot extract $archive.")
      }
    }

    private def listZip(archive: File): Seq[File] = {
      val suppressStdErr = ProcessLogger(_ => ())
      val zipList = Process(
        Seq("zip", "-l", archive.toPath.toAbsolutePath.normalize.toString)
      )
      zipList.lineStream(suppressStdErr).map(file)
    }

    private def extractTarGz(archive: File, root: File): Unit = {
      IO.createDirectory(root)
      val exitCode = Process(
        Seq(
          "tar",
          "xf",
          archive.toPath.toAbsolutePath.toString
        ),
        cwd = Some(root)
      ).!
      if (exitCode != 0) {
        throw new RuntimeException(s"Cannot extract $archive.")
      }
    }

    private def listTarGz(archive: File): Seq[File] = {
      val suppressStdErr = ProcessLogger(_ => ())
      val tarList =
        Process(Seq("tar", "tf", archive.toPath.toAbsolutePath.toString))
      tarList.lineStream(suppressStdErr).map(file)
    }

    private def extract(archive: File, root: File): Unit = {
      if (archive.getName.endsWith("zip")) {
        extractZip(archive, root)
      } else {
        extractTarGz(archive, root)
      }
    }

    private def list(archive: File): Seq[File] = {
      if (archive.getName.endsWith("zip")) {
        listZip(archive)
      } else {
        listTarGz(archive)
      }
    }

    private def graalArchive(os: OS, architecture: Architecture): File = {
      val packageDir =
        artifactRoot / s"graalvm-$graalVersion-${os.name}-${architecture.name}"
      if (!packageDir.exists()) {
        IO.createDirectory(packageDir)
      }
      val archiveName = s"graalvm-${os.name}-${architecture.name}-" +
        s"$graalVersion-$graalJavaVersion"
      packageDir / (archiveName + os.archiveExt)
    }

    private def downloadGraal(
      log: ManagedLogger,
      os: OS,
      architecture: Architecture
    ): File = {
      val archive = graalArchive(os, architecture)
      if (!archive.exists()) {
        log.info(
          s"Downloading GraalVM $graalVersion Java $graalJavaVersion " +
          s"for $os $architecture"
        )
        val graalUrl =
          s"https://github.com/graalvm/graalvm-ce-builds/releases/download/" +
          s"vm-$graalVersion/" +
          s"graalvm-ce-java$graalJavaVersion-${os.graalName}-" +
          s"${architecture.name}-$graalVersion${os.archiveExt}"
        val exitCode = (url(graalUrl) #> archive).!
        if (exitCode != 0) {
          throw new RuntimeException(s"Graal download from $graalUrl failed.")
        }
      }

      archive
    }

    private def copyGraal(
      os: OS,
      architecture: Architecture,
      runtimeDir: File
    ): Unit = {
      val archive = graalArchive(os, architecture)
      extract(archive, runtimeDir)
    }

    /** Prepare the GraalVM package.
      *
      * @param log the logger
      * @param os the system type
      * @param architecture the architecture type
      * @return the path to the created GraalVM package
      */
    def createGraalPackage(
      log: ManagedLogger,
      os: OS,
      architecture: Architecture
    ): File = {
      log.info("Building GraalVM distribution")
      val archive = downloadGraal(log, os, architecture)

      if (os.hasSupportForSulong) {
        val packageDir        = archive.getParentFile
        val archiveRootDir    = list(archive).head.getTopDirectory.getName
        val extractedGraalDir = packageDir / archiveRootDir
        if (extractedGraalDir.exists()) {
          IO.delete(extractedGraalDir)
        }

        log.info(s"Extracting $archive to $packageDir")
        extract(archive, packageDir)

        log.info("Installing components")
        gu(log, os, extractedGraalDir, "install", "python", "R")

        log.info(s"Re-creating $archive")
        IO.delete(archive)
        makeArchive(packageDir, archiveRootDir, archive)

        log.info(s"Cleaning up $extractedGraalDir")
        IO.delete(extractedGraalDir)
      }
      archive
    }

    /** Run the `gu` executable from the GraalVM distribution.
      *
      * @param log the logger
      * @param os the system type
      * @param graalDir the directory with a GraalVM distribution
      * @param arguments the command arguments
      * @return Stdout from the `gu` command.
      */
    def gu(
      log: ManagedLogger,
      os: OS,
      graalDir: File,
      arguments: String*
    ): String = {
      val shallowFile = graalDir / "bin" / "gu"
      val deepFile    = graalDir / "Contents" / "Home" / "bin" / "gu"
      val executableFile = os match {
        case OS.Linux =>
          shallowFile
        case OS.MacOS =>
          if (deepFile.exists) {
            deepFile
          } else {
            shallowFile
          }
        case OS.Windows =>
          graalDir / "bin" / "gu.cmd"
      }
      val javaHomeFile = executableFile.getParentFile.getParentFile
      val javaHome     = javaHomeFile.toPath.toAbsolutePath
      val command =
        executableFile.toPath.toAbsolutePath.toString +: arguments

      log.debug(
        s"Running $command in $graalDir with JAVA_HOME=${javaHome.toString}"
      )

      try {
        Process(
          command,
          Some(graalDir),
          ("JAVA_HOME", javaHome.toString),
          ("GRAALVM_HOME", javaHome.toString)
        ).!!
      } catch {
        case _: RuntimeException =>
          throw new RuntimeException(
            s"Failed to run '${command.mkString(" ")}'"
          )
      }
    }

    def copyEngine(os: OS, architecture: Architecture, distDir: File): Unit = {
      val engine = builtArtifact("engine", os, architecture)
      if (!engine.exists()) {
        throw new IllegalStateException(
          s"Cannot create bundle for $os / $architecture because corresponding " +
          s"engine has not been built."
        )
      }

      IO.copyDirectory(engine / s"enso-$ensoVersion", distDir / ensoVersion)
    }

    def makeExecutable(file: File): Unit = {
      val ownerOnly = false
      file.setExecutable(true, ownerOnly)
    }

    def fixLauncher(root: File, os: OS): Unit = {
      makeExecutable(root / "enso" / "bin" / os.executableName("enso"))
      IO.createDirectories(
        Seq("dist", "config", "runtime").map(root / "enso" / _)
      )
    }

    def makeArchive(root: File, rootDir: String, target: File): Unit = {
      val exitCode = if (target.getName.endsWith("zip")) {
        Process(
          Seq(
            "zip",
            "-9",
            "-q",
            "-r",
            target.toPath.toAbsolutePath.normalize.toString,
            rootDir
          ),
          cwd = Some(root)
        ).!
      } else {
        Process(
          Seq(
            "tar",
            "--use-compress-program=gzip -9",
            "-cf",
            target.toPath.toAbsolutePath.normalize.toString,
            rootDir
          ),
          cwd = Some(root)
        ).!
      }
      if (exitCode != 0) {
        throw new RuntimeException(s"Failed to create archive $target")
      }
    }

    /** Path to an arbitrary built artifact. */
    def builtArtifact(
      component: String,
      os: OS,
      architecture: Architecture
    ): File = artifactRoot / artifactName(component, os, architecture)

    /** Path to the artifact that is built on this local machine. */
    def localArtifact(component: String): File = {
      val architecture = Architecture.X64
      val os =
        if (Platform.isWindows) OS.Windows
        else if (Platform.isLinux) OS.Linux
        else if (Platform.isMacOS) OS.MacOS
        else throw new IllegalStateException("Unknown OS")
      artifactRoot / artifactName(component, os, architecture)
    }

    /** Path to a built archive.
      *
      * These archives are built by [[makePackages]] and [[makeBundles]].
      */
    def builtArchive(
      component: String,
      os: OS,
      architecture: Architecture
    ): File =
      artifactRoot / (artifactName(
        component,
        os,
        architecture
      ) + os.archiveExt)

    private def cleanDirectory(dir: File): Unit = {
      for (f <- IO.listFiles(dir)) {
        IO.delete(f)
      }
    }

    /** Creates compressed and ready for release packages for the launcher and
      * engine.
      *
      * A project manager package is not created, as we release only its bundle.
      * See [[makeBundles]].
      *
      * It does not trigger any builds. Instead, it uses available artifacts
      * placed in `artifactRoot`. These artifacts may be created using the
      * `enso/build*Distribution` tasks or they may come from other workers (as
      * is the case in the release CI where the artifacts are downloaded from
      * other jobs).
      */
    def makePackages = Command.command("makePackages") { state =>
      val log = state.log
      for {
        os   <- OS.platforms
        arch <- Architecture.archs
      } {
        val launcher = builtArtifact("launcher", os, arch)
        if (launcher.exists()) {
          fixLauncher(launcher, os)
          val archive = builtArchive("launcher", os, arch)
          makeArchive(launcher, "enso", archive)
          log.info(s"Created $archive")
        }

        val engine = builtArtifact("engine", os, arch)
        if (engine.exists()) {
          if (os.isUNIX) {
            makeExecutable(engine / s"enso-$ensoVersion" / "bin" / "enso")
          }
          val archive = builtArchive("engine", os, arch)
          makeArchive(engine, s"enso-$ensoVersion", archive)
          log.info(s"Created $archive")
        }
      }
      state
    }

    /** Creates launcher and project-manager bundles that include the component
      * itself, the engine and a Graal runtime.
      *
      * It will download the GraalVM runtime and cache it in `artifactRoot` so
      * further invocations for the same version will not need to download it.
      *
      * It does not trigger any builds. Instead, it uses available artifacts
      * placed in `artifactRoot`. These artifacts may be created using the
      * `enso/build*Distribution` tasks or they may come from other workers (as
      * is the case in the release CI where the artifacts are downloaded from
      * other jobs).
      */
    def makeBundles = Command.command("makeBundles") { state =>
      val log = state.log
      for {
        os   <- OS.platforms
        arch <- Architecture.archs
      } {
        val launcher = builtArtifact("launcher", os, arch)
        if (launcher.exists()) {
          fixLauncher(launcher, os)
          copyEngine(os, arch, launcher / "enso" / "dist")
          copyGraal(os, arch, launcher / "enso" / "runtime")

          val archive = builtArchive("bundle", os, arch)
          makeArchive(launcher, "enso", archive)

          cleanDirectory(launcher / "enso" / "dist")
          cleanDirectory(launcher / "enso" / "runtime")

          log.info(s"Created $archive")
        }

        val pm = builtArtifact("project-manager", os, arch)
        if (pm.exists()) {
          if (os.isUNIX) {
            makeExecutable(pm / "enso" / "bin" / "project-manager")
          }

          copyEngine(os, arch, pm / "enso" / "dist")
          copyGraal(os, arch, pm / "enso" / "runtime")

          IO.copyFile(
            file("distribution/enso.bundle.template"),
            pm / "enso" / ".enso.bundle"
          )

          val archive = builtArchive("project-manager", os, arch)
          makeArchive(pm, "enso", archive)

          cleanDirectory(pm / "enso" / "dist")
          cleanDirectory(pm / "enso" / "runtime")

          log.info(s"Created $archive")
        }
      }
      state
    }
  }
}
