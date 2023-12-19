package org.enso.runtimeversionmanager.components

import org.enso.cli.OS

/** Component configuration of the GraalVM distribution. */
class GraalVMComponentConfiguration extends RuntimeComponentConfiguration {

  import GraalVMComponentConfiguration._

  /** @inheritdoc */
  override def getRequiredComponents(
    version: GraalVMVersion,
    os: OS
  ): Seq[GraalVMComponent] = {
    val optPythonComponent =
      if (os.hasPythonSupport) Seq(GraalVMComponent.python) else Seq()
    val optRComponent =
      if (os.hasRSupport) Seq(GraalVMComponent.R) else Seq()
    if (version.isUnchained) {
      Seq()
    } else {
      version.graalVersion match {
        case GraalVersions.Major(v) if v >= 23 =>
          // Since 23.0.0, R is not bundled in the Graal release anymore.
          Seq(GraalVMComponent.js) ++ optPythonComponent
        case GraalVersions.Major(v) if v >= 22 =>
          Seq(GraalVMComponent.js) ++ optRComponent ++ optPythonComponent
        case GraalVersions.Major(v)
            if v > 20 && os.hasSulongSupport && os.hasRSupport =>
          Seq(GraalVMComponent.R) ++ optPythonComponent
        case _ =>
          Seq()
      }
    }
  }

}
object GraalVMComponentConfiguration {

  /** OS extensions. */
  implicit private class OSExtensions(os: OS) {

    /** Check if the provided OS supports Sulong runtime.
      *
      * Sulong is a Graal sub-project, providing an engine for running
      * LLVM bitcode on GraalVM.
      *
      * @return `true` if the OS supports Sulong runtime and `false` otherwise
      */
    def hasSulongSupport: Boolean =
      os match {
        case OS.Linux   => true
        case OS.MacOS   => true
        case OS.Windows => false
      }

    /** Check if the provided OS supports Python.
      * Python is currently not supported in any form on Windows.
      *
      * @return `true` if the OS supports Python runtime
      */
    def hasPythonSupport: Boolean =
      os match {
        case OS.Windows => false
        case _          => true
      }

    /** Check if the provided OS supports FastR.
      * FastR is currently not supported in any form on Windows.
      *
      * @return `true` if the OS supports FastR GraalVM component.
      */
    def hasRSupport: Boolean =
      os match {
        case OS.Windows => false
        case _          => true
      }
  }

  private object GraalVersions {

    /** Get the major Graal version number. */
    object Major {
      def unapply(version: String): Option[Int] = {
        version.takeWhile(_ != '.').toIntOption
      }
    }
  }
}
