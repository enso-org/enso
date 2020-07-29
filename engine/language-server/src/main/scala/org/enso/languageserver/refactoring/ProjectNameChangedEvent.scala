package org.enso.languageserver.refactoring

import org.enso.languageserver.event.Event

/**
  * An event notifying that project name has changed.
  *
  * @param oldName the old name of the project
  * @param newName the new project name
  */
case class ProjectNameChangedEvent(oldName: String, newName: String)
    extends Event
