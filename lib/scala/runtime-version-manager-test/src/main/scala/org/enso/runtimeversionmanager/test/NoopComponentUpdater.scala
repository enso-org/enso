package org.enso.runtimeversionmanager.test

import org.enso.runtimeversionmanager.components.{
  GraalVMComponent,
  RuntimeComponentUpdater
}

import scala.util.Try

/** Test component updater that does not do anything. */
object NoopComponentUpdater extends RuntimeComponentUpdater {

  /** @inheritdoc */
  override def list(): Try[Seq[GraalVMComponent]] =
    Try(Seq())

  /** @inheritdoc */
  override def install(components: Seq[GraalVMComponent]): Try[Unit] =
    Try(())
}
