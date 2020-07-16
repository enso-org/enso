package org.enso.languageserver.refactoring

import org.enso.languageserver.event.Event

/**
  * An event notifying that project name has changed.
  *
  * @param name the new project name
  */
case class ProjectNameChangedEvent(name: String) extends Event
