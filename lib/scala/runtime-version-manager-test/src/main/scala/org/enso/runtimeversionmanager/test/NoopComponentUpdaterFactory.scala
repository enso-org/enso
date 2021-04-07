package org.enso.runtimeversionmanager.test

import org.enso.runtimeversionmanager.OS
import org.enso.runtimeversionmanager.components.{
  GraalRuntime,
  RuntimeComponentUpdater,
  RuntimeComponentUpdaterFactory
}

/** Test factory creating a noop updater. */
object NoopComponentUpdaterFactory extends RuntimeComponentUpdaterFactory {

  /** @inheritdoc */
  override def build(runtime: GraalRuntime, os: OS): RuntimeComponentUpdater =
    NoopComponentUpdater
}
