package org.enso.runtimeversionmanager.components

/** The factory that creates a runtime component updater. */
trait RuntimeComponentUpdaterFactory {

  /** Create a runtime component updater.
    *
    * @param runtime the GraalVM runtime
    * @return new instance of the runtime component updater
    */
  def build(runtime: GraalRuntime): RuntimeComponentUpdater
}

object RuntimeComponentUpdaterFactory {

  /** The default runtime component updater factory creating an instance of
    * [[GraalVMComponentUpdater]].
    */
  object Default extends RuntimeComponentUpdaterFactory {

    /** @inheritdoc */
    override def build(runtime: GraalRuntime): RuntimeComponentUpdater =
      new GraalVMComponentUpdater(runtime)
  }
}
