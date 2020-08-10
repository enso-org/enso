package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer

/**
  * Version information identifying the runtime that can be used with an engine
  * release.
  *
  * @param graal version of the GraalVM
  * @param java Java version of the GraalVM flavour that should be used
  */
case class RuntimeVersion(graal: SemVer, java: String) {

  /**
    * @inheritdoc
    */
  override def toString: String = s"GraalVM $graal Java $java"
}
