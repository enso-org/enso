package org.enso.build.stdlibupdater

import org.enso.pkg.PackageManager

import java.nio.file.{Files, Path}
import scala.collection.Factory
import scala.jdk.StreamConverters.StreamHasToScala
import scala.util.Using

/** A helper tool for checking standard library versions and possibly updating
  * them.
  */
class StdlibWalker(
  bundledLibRoot: Path,
  targetVersion: String,
  visitor: StdlibVisitor
) {

  /** Walks the directory structure of the standard library and if any versions
    * are mismatched, calls the [[visitor]].
    */
  def walk(): Unit = {
    val standardRoot = bundledLibRoot.resolve("Standard")

    for (libraryRoot <- listDirectory(standardRoot)) {
      val libraryName  = libraryRoot.getFileName.toString
      val versionRoots = listDirectory(libraryRoot)
      versionRoots match {
        case Seq(versionRoot) =>
          checkPackageVersion(libraryName, versionRoot)
          checkVersionDirectory(libraryRoot, libraryName, versionRoot)
        case _ =>
          throw new IllegalStateException(
            s"Exactly one version directory expected in [$libraryRoot], " +
            s"but found: $versionRoots."
          )
      }
    }
  }

  private def checkPackageVersion(libraryName: String, root: Path): Unit = {
    val pkg            = PackageManager.Default.loadPackage(root.toFile).get
    val currentVersion = pkg.config.version
    if (currentVersion == targetVersion) {
      println(s"Package of [$libraryName] has the correct version.")
    } else {
      visitor.packageVersionMismatch(
        libraryName     = libraryName,
        currentVersion  = currentVersion,
        expectedVersion = targetVersion,
        pkg             = pkg
      )
    }
  }

  private def checkVersionDirectory(
    libraryRoot: Path,
    libraryName: String,
    versionRoot: Path
  ): Unit = {
    val version = versionRoot.getFileName.toString
    if (version == targetVersion) {
      println(
        s"[$libraryName] directory is in the correct version."
      )
    } else {
      val newRoot = libraryRoot.resolve(targetVersion)
      visitor.directoryMismatch(
        libraryName       = libraryName,
        currentDirectory  = versionRoot,
        expectedDirectory = newRoot
      )
    }
  }

  private def listDirectory(dir: Path): Seq[Path] =
    Using(Files.list(dir))(_.toScala(Factory.arrayFactory).toSeq).get
}
