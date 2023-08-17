package org.enso.languageserver.refactoring

object RefactoringProtocol {

  /** Notification signaling about the project being renamed.
    *
    * @param newName the new name of the project
    */
  case class ProjectRenamedNotification(newName: String)
}
