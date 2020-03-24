package org.enso.projectmanager.infrastructure.repository

/**
  * Base interface for repository failures.
  */
sealed trait ProjectRepositoryFailure

object ProjectRepositoryFailure {

  /**
    * Signals that repository cannot load project index.
    * @param msg an error message
    */
  case class CannotLoadIndex(msg: String) extends ProjectRepositoryFailure

  /**
    * Signals problems with underlying storage.
    *
    * @param msg an error message
    */
  case class StorageFailure(msg: String) extends ProjectRepositoryFailure

  /**
    * Signals that project is not present in the storage.
    */
  case object ProjectNotFoundInIndex extends ProjectRepositoryFailure

  /**
    * Signals that precondition is not met for stored data.
    *
    * @param msg an error message
    */
  case class InconsistentStorage(msg: String) extends ProjectRepositoryFailure

}
