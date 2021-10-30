package org.enso.runtimeversionmanager.components

import scala.util.Try

/** Module that manages components of the runtime distribution. */
trait RuntimeComponentUpdater {

  /** List the installed runtime components.
    *
    * @return the list of installed runtime components
    */
  def list: Try[Seq[GraalVMComponent]]

  /** Install the provided runtime components.
    *
    * @param components the list of components to install
    */
  def install(components: Seq[GraalVMComponent]): Try[Unit]
}
