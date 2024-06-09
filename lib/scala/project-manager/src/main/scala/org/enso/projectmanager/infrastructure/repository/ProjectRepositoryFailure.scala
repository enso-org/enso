package org.enso.projectmanager.infrastructure.repository

/** Base interface for repository failures.
  */
sealed trait ProjectRepositoryFailure {

  /** The error message. */
  def message: String
}

object ProjectRepositoryFailure {

  /** Signals that repository cannot load project index.
    * @param message an error message
    */
  case class CannotLoadIndex(message: String) extends ProjectRepositoryFailure

  /** Signals problems with underlying storage.
    *
    * @param message an error message
    */
  case class StorageFailure(message: String) extends ProjectRepositoryFailure

  /** Signals that project is not present in the storage.
    */
  case object ProjectNotFoundInIndex extends ProjectRepositoryFailure {
    override def message = "Project not foun in index"
  }

  /** Signals that precondition is not met for stored data.
    *
    * @param message an error message
    */
  case class InconsistentStorage(message: String)
      extends ProjectRepositoryFailure

}
