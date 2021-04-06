package org.enso.runtimeversionmanager.components

import org.enso.runtimeversionmanager.OS

/** The factory that creates a runtime component updater. */
trait RuntimeComponentUpdaterFactory {

  /** Create a runtime component updater.
    *
    * @param runtime the GraalVM runtime
    * @param os the operating system
    * @return new instance of the runtime component updater
    */
  def build(runtime: GraalRuntime, os: OS): RuntimeComponentUpdater
}

object RuntimeComponentUpdaterFactory {

  /** The default runtime component updater factory creating an instance of
    * [[GraalVMComponentUpdater]].
    */
  object Default extends RuntimeComponentUpdaterFactory {

    /** @inheritdoc */
    override def build(runtime: GraalRuntime, os: OS): RuntimeComponentUpdater =
      new GraalVMComponentUpdater(runtime, os)
  }
}
