package org.enso.runtimeversionmanager.components

/** A component of the GraalVM distribution. */
case class GraalVMComponent(id: String)

object GraalVMComponent {

  val js: GraalVMComponent     = GraalVMComponent("js")
  val python: GraalVMComponent = GraalVMComponent("python")
  val R: GraalVMComponent      = GraalVMComponent("R")
}
