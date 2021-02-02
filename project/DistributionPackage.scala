import sbt.internal.util.ManagedLogger
import sbt._
import sbt.io.syntax.fileToRichFile
import sbt.util.{CacheStore, CacheStoreFactory, FileInfo, Tracked}

import scala.sys.process._

object DistributionPackage {
  def copyDirectoryIncremental(
    source: File,
    destination: File,
    cache: CacheStore
  ): Unit = {
    val allFiles = source.allPaths.get().toSet
    Tracked.diffInputs(cache, FileInfo.lastModified)(allFiles) { diff =>
      val missing = diff.unmodified.exists { f =>
        val destinationFile = destination / f.getName
        !destinationFile.exists()
      }
      if (diff.modified.nonEmpty || diff.removed.nonEmpty || missing) {
        IO.delete(destination)
        IO.copyDirectory(source, destination)
      }
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
    graalVersion: String,
    javaVersion: String
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

    copyDirectoryIncremental(
      file("distribution/std-lib"),
      distributionRoot / "std-lib",
      cacheFactory.make("engine-std-lib")
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
    def name: String
    def graalName: String                    = name
    def executableName(base: String): String = base
    def archiveExt: String                   = ".tar.gz"
    def isUNIX: Boolean                      = true
  }
  object OS {
    case object Linux extends OS {
      override def name: String = "linux"
    }
    case object MacOS extends OS {
      override def name: String      = "macos"
      override def graalName: String = "darwin"
    }
    case object Windows extends OS {
      override def name: String                         = "windows"
      override def executableName(base: String): String = base + ".exe"
      override def archiveExt: String                   = ".zip"
      override def isUNIX: Boolean                      = false
    }

    val platforms = Seq(Linux, MacOS, Windows)
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

    private def extract(archive: File, root: File): Unit = {
      if (archive.getName.endsWith("zip")) {
        extractZip(archive, root)
      } else {
        extractTarGz(archive, root)
      }
    }

    def copyGraal(
      log: ManagedLogger,
      os: OS,
      architecture: Architecture,
      runtimeDir: File
    ): Unit = {
      val packageName = s"graalvm-${os.name}-${architecture.name}-" +
        s"$graalVersion-$graalJavaVersion"
      val root = artifactRoot / packageName
      if (!root.exists()) {
        log.info(
          s"Downloading GraalVM $graalVersion Java $graalJavaVersion " +
          s"for $os $architecture"
        )
        val graalUrl =
          s"https://github.com/graalvm/graalvm-ce-builds/releases/download/" +
          s"vm-$graalVersion/" +
          s"graalvm-ce-java$graalJavaVersion-${os.graalName}-" +
          s"${architecture.name}-$graalVersion${os.archiveExt}"
        val archive  = artifactRoot / (packageName + os.archiveExt)
        val exitCode = (url(graalUrl) #> archive).!
        if (exitCode != 0) {
          throw new RuntimeException(s"Graal download from $graalUrl failed.")
        }

        extract(archive, root)
      }

      IO.copyDirectory(
        root / graalInPackageName,
        runtimeDir / graalInPackageName
      )
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
            "-czf",
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

    private def cleanDirectory(dir: File): Unit = {
      for (f <- IO.listFiles(dir)) {
        IO.delete(f)
      }
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
          copyGraal(log, os, arch, launcher / "enso" / "runtime")

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
          copyGraal(log, os, arch, pm / "enso" / "runtime")
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
