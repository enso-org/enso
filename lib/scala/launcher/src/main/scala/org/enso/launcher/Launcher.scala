package org.enso.launcher

import buildinfo.Info
import java.nio.file.Path

import org.enso.pkg.PackageManager

object Launcher {
  private val packageManager = PackageManager.Default

  private def workingDirectory: Path = Path.of(".")

  def newProject(name: String, path: Option[Path]): Unit = {
    // TODO [RW] this is not the final implementation
    val actualPath = path.getOrElse(workingDirectory.resolve(name))
    packageManager.create(actualPath.toFile, name)
    println(s"Project created in $actualPath")
  }

  def displayVersion(useJson: Boolean): Unit = {
    val version   = Info.ensoVersion // Note [Launcher Version]
    val osArch    = System.getProperty("os.arch")
    val osName    = System.getProperty("os.name")
    val osVersion = System.getProperty("os.version")

    val runtimeVersion = // TODO [RW] add with #976
      if (useJson) "\"<not implemented yet>\""
      else "Runtime component is not yet implemented in the launcher."

    val versionOutput =
      if (useJson) {
        s"""{ "version": "$version",
           |  "branch": "${Info.branch}",
           |  "dirty": ${Info.isDirty},
           |  "commit": "${Info.commit}",
           |  "osName": "$osName",
           |  "osVersion": "$osVersion",
           |  "osArch": "$osArch",
           |  "runtime": $runtimeVersion
           |}""".stripMargin
      } else {
        val dirtyStr = if (Info.isDirty) {
          "*"
        } else {
          ""
        }

        s"""
           |Enso Launcher
           |Version:    $version
           |Built with: scala-${Info.scalacVersion} and GraalVM ${Info.graalVersion} Native Image
           |Built from: ${Info.branch}$dirtyStr @ ${Info.commit}
           |Running on: $osName $osVersion ($osArch)
           |Currently selected Enso version:
           |$runtimeVersion
           |""".stripMargin
      }
    println(versionOutput)
  }
}

/* Note [Launcher Version]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * Currently the launcher is released along with new Enso versions, so its
 * vesrsion number is tied to Enso version number (although a given launcher
 * version can be used with different Enso versions other than the one it was
 * released with, unless an Enso version has a higher minimum launcher version).
 */
