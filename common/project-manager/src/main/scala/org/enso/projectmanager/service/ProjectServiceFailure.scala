package org.enso.projectmanager.service

/**
  * Base interface for project service failures.
  */
sealed trait ProjectServiceFailure

object ProjectServiceFailure {

  /**
    * Signals validation failures.
    *
    * @param msg a message
    */
  case class ValidationFailure(msg: String) extends ProjectServiceFailure

  /**
    * Signals problems with underlying data store.
    *
    * @param msg a message
    */
  case class DataStoreFailure(msg: String) extends ProjectServiceFailure

  /**
    * Signals that the project already exists.
    */
  case object ProjectExists extends ProjectServiceFailure

  /**
    * Signals that the project doesn't exist.
    */
  case object ProjectNotFound extends ProjectServiceFailure

}
