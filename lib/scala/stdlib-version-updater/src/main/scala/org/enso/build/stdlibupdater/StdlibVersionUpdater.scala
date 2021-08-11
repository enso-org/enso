package org.enso.build.stdlibupdater

import org.enso.pkg.PackageManager

import java.nio.file.{Files, Path}
import scala.collection.Factory
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Using
import scala.util.control.NonFatal

object StdlibVersionUpdater {
  def main(args: Array[String]): Unit = try {
    updateStandardLibraryVersions()
  } catch {
    case NonFatal(error) =>
      println(s"Failed: $error")
      error.printStackTrace()
      sys.exit(1)
  }

  def updateStandardLibraryVersions(): Unit = {
    val bundledLibRoot = Path.of("distribution/lib")
    val standardRoot   = bundledLibRoot.resolve("Standard")

    for (libraryRoot <- listDirectory(standardRoot)) {
      val libraryName  = libraryRoot.getFileName.toString
      val versionRoots = listDirectory(libraryRoot)
      versionRoots match {
        case Seq(versionRoot) =>
          updatePackageVersion(libraryName, versionRoot)
          updateVersionDirectory(libraryRoot, libraryName, versionRoot)
        case _ =>
          throw new IllegalStateException(
            s"Exactly one version directory expected in [$libraryRoot], " +
            s"but found: $versionRoots."
          )
      }
    }
  }

  private val targetVersion: String = buildinfo.Info.stdLibVersion

  private def updatePackageVersion(libraryName: String, root: Path): Unit = {
    val pkg            = PackageManager.Default.loadPackage(root.toFile).get
    val currentVersion = pkg.config.version
    if (currentVersion == targetVersion) {
      println(s"Package of [$libraryName] already has the correct version.")
    } else {
      pkg.updateConfig(config => config.copy(version = targetVersion))
      println(
        s"Updated config of [$libraryName] from [$currentVersion] to " +
        s"[$targetVersion]."
      )
    }
  }

  private def updateVersionDirectory(
    libraryRoot: Path,
    libraryName: String,
    versionRoot: Path
  ): Unit = {
    val version = versionRoot.getFileName.toString
    if (version == targetVersion) {
      println(
        s"[$libraryName] directory is already in the correct version."
      )
    } else {
      val newRoot = libraryRoot.resolve(targetVersion)
      Files.move(versionRoot, newRoot)
      println(
        s"Moved [$libraryName] from [$version] to [$targetVersion]."
      )
    }
  }

  private def listDirectory(dir: Path): Seq[Path] =
    Using(Files.list(dir))(_.toScala(Factory.arrayFactory).toSeq).get
}
