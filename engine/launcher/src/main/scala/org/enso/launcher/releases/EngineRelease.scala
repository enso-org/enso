package org.enso.launcher.releases

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.OS
import org.enso.launcher.components.Manifest

/**
  * Represents an engine release.
  *
  * @param version engine version
  * @param manifest manifest associated with the release
  * @param isBroken specifies whether this release is marked as broken
  * @param release a [[Release]] that allows to download assets
  */
case class EngineRelease(
  version: SemVer,
  manifest: Manifest,
  isBroken: Boolean,
  release: Release
) {

  /**
    * Determines the filename of the package that should be downloaded from this
    * release.
    *
    * That filename may be platform specific.
    */
  def packageFileName: String = {
    val os = OS.operatingSystem match {
      case OS.Linux   => "linux"
      case OS.MacOS   => "macos"
      case OS.Windows => "windows"
    }
    val arch = OS.architecture
    val extension = OS.operatingSystem match {
      case OS.Linux   => ".tar.gz"
      case OS.MacOS   => ".tar.gz"
      case OS.Windows => ".zip"
    }
    s"enso-engine-$version-$os-$arch$extension"
  }
}
