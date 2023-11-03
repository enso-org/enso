package org.enso.runtimeversionmanager.components

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
