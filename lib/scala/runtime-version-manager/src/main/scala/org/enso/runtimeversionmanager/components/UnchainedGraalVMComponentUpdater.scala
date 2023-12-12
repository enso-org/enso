package org.enso.runtimeversionmanager.components

import scala.util.{Success, Try}

/** A dummy component updater for [[GraalVMVersion.isUnchained unchained]] GraalVM.
  * There is no `gu` utility in unchained GraalVM, so this updater does not do anything.
  * It only lists the components that are known to be included in the unchained GraalVM.
  */
class UnchainedGraalVMComponentUpdater extends RuntimeComponentUpdater {

  /** There
    *
    * @return the list of installed runtime components
    */
  override def list(): Try[Seq[GraalVMComponent]] = Success(Seq())

  /** Install the provided runtime components.
    *
    * @param components the list of components to install
    */
  override def install(components: Seq[GraalVMComponent]): Try[Unit] = {
    throw new IllegalStateException(
      "Cannot install components in unchained GraalVM"
    )
  }
}
