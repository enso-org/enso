package org.enso.runtimeversionmanager.runner

/** Returned by [[Runner.version]], specifies if the engine that is queried for
  * version is from a project or the default one.
  */
sealed trait WhichEngine
object WhichEngine {
  case class FromProject(name: String) extends WhichEngine
  case object Default                  extends WhichEngine
}
