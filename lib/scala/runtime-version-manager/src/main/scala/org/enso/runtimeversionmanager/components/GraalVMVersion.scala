package org.enso.runtimeversionmanager.components

/** Version information identifying the runtime that can be used with an engine
  * release.
  *
  * @param graalVersion version of the GraalVM
  * @param java Java version of the GraalVM flavour that should be used
  */
case class GraalVMVersion(graalVersion: String, java: String) {

  /** @inheritdoc
    */
  override def toString: String = s"GraalVM $graalVersion Java $java"
}
