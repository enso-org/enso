package org.enso.build.stdlibupdater

import java.nio.file.{Files, Path}
import org.enso.pkg.Package

import java.io.File

/** Defines callbacks for handling version mismatches. */
trait StdlibVisitor {

  /** Called when the directory representing the library version is different
    * than expected.
    */
  def directoryMismatch(
    libraryName: String,
    currentDirectory: Path,
    expectedDirectory: Path
  ): Unit

  /** Called when the version in package config is different than expected. */
  def packageVersionMismatch(
    libraryName: String,
    currentVersion: String,
    expectedVersion: String,
    pkg: Package[File]
  ): Unit
}

/** A [[StdlibVisitor]] that updates the directories and configs to make sure
  * that the versions are correct.
  */
class UpdatingVisitor(shouldFormat: Boolean) extends StdlibVisitor {

  /** @inheritdoc */
  override def directoryMismatch(
    libraryName: String,
    currentDirectory: Path,
    expectedDirectory: Path
  ): Unit = {
    Files.move(currentDirectory, expectedDirectory)
    println(
      s"Moved [$libraryName] from [${currentDirectory.getFileName}] to " +
      s"[${expectedDirectory.getFileName}]."
    )
  }

  /** @inheritdoc */
  override def packageVersionMismatch(
    libraryName: String,
    currentVersion: String,
    targetVersion: String,
    pkg: Package[File]
  ): Unit = {
    pkg.updateConfig(config => config.copy(version = targetVersion))
    if (shouldFormat) { Prettier.format(pkg.configFile.toPath) }
    println(
      s"Updated config of [$libraryName] from [$currentVersion] to " +
      s"[$targetVersion]."
    )
  }
}

/** A [[StdlibVisitor]] that throws an exception on the first mismatch. */
object ReportingVisitor extends StdlibVisitor {

  /** An exception thrown when the version is different than expected. */
  case class VersionMismatch(message: String) extends RuntimeException(message)

  /** @inheritdoc */
  override def directoryMismatch(
    libraryName: String,
    currentDirectory: Path,
    expectedDirectory: Path
  ): Unit = throw VersionMismatch(
    s"[$libraryName] has directory [${currentDirectory.getFileName}], " +
    s"but it should be [${expectedDirectory.getFileName}]."
  )

  /** @inheritdoc */
  override def packageVersionMismatch(
    libraryName: String,
    currentVersion: String,
    expectedVersion: String,
    pkg: Package[File]
  ): Unit = throw VersionMismatch(
    s"Config of [$libraryName] has version [$currentVersion], " +
    s"but it should be [$expectedVersion]."
  )
}
