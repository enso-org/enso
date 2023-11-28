package org.enso.runtimeversionmanager.components

import nl.gn0s1s.bump.SemVer

/** Version information identifying the runtime that can be used with an engine
  * release.
  *
  * @param graalVersion version of the GraalVM. Can be specified as a semantic
  *                     version.
  * @param javaVersion Java version of the GraalVM flavour that should be used.
  *                    Can be specified either as a single integer or as a
  *                    semantic version
  */
case class GraalVMVersion(graalVersion: String, javaVersion: String) {
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

  /** The GraalVM distribution policy changed a lot since GraalVM 23.1.0 for JDK 21.
    * Most of the components for the newest GraalVM distributions are distributed as
    * artifacts from the Maven central. This mens there is no longer `gu` tool.
    *
    * @see https://medium.com/graalvm/truffle-unchained-13887b77b62c
    * @return true if this version is associated with Truffle unchained.
    */
  def isUnchained: Boolean = {
    javaMajorVersion >= 21 && graalMajorVersion >= 23
  }
}

object GraalVMVersion {
  def isCorrectVersionFormat(version: String): Boolean = {
    version.toIntOption match {
      case Some(_) => true
      case None =>
        SemVer(version) match {
          case Some(_) => true
          case None =>
            version.matches("^(\\d+\\.){3}\\d+$")
        }
    }
  }
}
