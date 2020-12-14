package org.enso.runtimeversionmanager.components

sealed trait ComponentSelector
object ComponentSelector {
  case object Launcher       extends ComponentSelector
  case object ProjectManager extends ComponentSelector
}
