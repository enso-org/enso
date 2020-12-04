import sbt.{file, singleFileFinder, File, IO}
import sbt.io.syntax.fileToRichFile
import sbt.util.{CacheStore, CacheStoreFactory, FileInfo, Tracked}

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
}
