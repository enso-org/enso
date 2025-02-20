package org.enso.runtimeversionmanager.components

import org.enso.semver.SemVer

/** Version information identifying the runtime that can be used with an engine
  * release.
  *
  * @param graalVersion version of the GraalVM. Can be specified as a semantic
  *                     version.
  * @param javaVersion Java version of the GraalVM flavour that should be used.
  *                    Can be specified either as a single integer or as a
  *                    semantic version
  */
case class GraalVMVersion(graalVersion: String, javaVersion: String)
    extends Comparable[GraalVMVersion] {
  require(GraalVMVersion.isCorrectVersionFormat(graalVersion))
  require(GraalVMVersion.isCorrectVersionFormat(javaVersion))

  /** @inheritdoc
    */
  override def toString: String = s"GraalVM $graalVersion Java $javaVersion"

  def graalMajorVersion: Int = graalVersion.split("\\.").head.toInt

  def javaMajorVersion: Int = {
    if (javaVersion.contains(".")) {
      javaVersion.split("\\.").head.toInt
    } else {
      javaVersion.toInt
    }
  }

  override def compareTo(other: GraalVMVersion): Int = {
    val javaSemVer      = SemVer.parse(javaVersion)
    val otherJavaSemVer = SemVer.parse(other.javaVersion)
    if (javaSemVer.isSuccess && otherJavaSemVer.isSuccess) {
      val comp = javaSemVer.get.compareTo(otherJavaSemVer.get)
      if (comp != 0) {
        return comp
      }
    }

    val graalSemVer      = SemVer.parse(graalVersion)
    val otherGraalSemVer = SemVer.parse(other.graalVersion)
    if (graalSemVer.isSuccess && otherGraalSemVer.isSuccess) {
      val comp = graalSemVer.get.compareTo(otherGraalSemVer.get)
      if (comp != 0) {
        return comp
      }
    }

    val javaMajorComp = javaMajorVersion.compareTo(other.javaMajorVersion)
    if (javaMajorComp != 0) {
      return javaMajorComp
    }
    val graalMajorComp = graalMajorVersion.compareTo(other.graalMajorVersion)
    if (graalMajorComp != 0) {
      return graalMajorComp
    }

    0
  }
}

object GraalVMVersion {
  def isCorrectVersionFormat(version: String): Boolean = {
    version.toIntOption match {
      case Some(_) => true
      case None =>
        SemVer
          .parse(version)
          .fold(_ => version.matches("^(\\d+\\.){3}\\d+$"), _ => true)
    }
  }
}
