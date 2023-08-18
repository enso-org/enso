package org.enso.languageserver.refactoring

object RefactoringProtocol {

  /** Notification signaling about the project being renamed.
    *
    * @param normalizedName the new normalized project name
    * @param name the new name of the project
    */
  case class ProjectRenamedNotification(normalizedName: String, name: String)
}
