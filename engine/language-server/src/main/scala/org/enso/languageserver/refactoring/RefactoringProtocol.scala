package org.enso.languageserver.refactoring

object RefactoringProtocol {

  /** Notification signaling about the project being renamed.
    *
    * @param oldNormalizedName old normalized name of the project
    * @param newNormalizedName new normalized name of the project
    * @param newName new display name of the project
    */
  case class ProjectRenamedNotification(
    oldNormalizedName: String,
    newNormalizedName: String,
    newName: String
  )
}
