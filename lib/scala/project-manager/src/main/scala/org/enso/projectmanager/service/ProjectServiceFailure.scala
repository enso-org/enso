package org.enso.projectmanager.service

/** Base interface for project service failures.
  */
sealed trait ProjectServiceFailure

object ProjectServiceFailure {

  /** Signals validation failures.
    *
    * @param msg a message
    */
  case class ValidationFailure(msg: String) extends ProjectServiceFailure

  /** Signals problems with underlying data store.
    *
    * @param msg a message
    */
  case class DataStoreFailure(msg: String) extends ProjectServiceFailure

  /** Signals that the project already exists.
    */
  case object ProjectExists extends ProjectServiceFailure

  /** Signals that the project doesn't exist.
    */
  case object ProjectNotFound extends ProjectServiceFailure

  /** Signals that a failure occurred during project startup.
    *
    * @param message a failure message
    */
  case class ProjectOpenFailed(message: String) extends ProjectServiceFailure

  /** Signals that a failure occurred during project shutdown.
    *
    * @param message a failure message
    */
  case class ProjectCloseFailed(message: String) extends ProjectServiceFailure

  /** Signals that operation cannot be executed, because a project is not open.
    */
  case object ProjectNotOpen extends ProjectServiceFailure

  /** Signals that the project cannot be closed, because other clients are
    * connected.
    */
  case object ProjectOpenByOtherPeers extends ProjectServiceFailure

  /** Signals that removal of project failed because one client still use it.
    */
  case object CannotRemoveOpenProject extends ProjectServiceFailure

  /** Signals operation timeout.
    */
  case object ProjectOperationTimeout extends ProjectServiceFailure

  /** Signals generic server failures.
    *
    * @param msg a description of a failure
    */
  case class LanguageServerFailure(msg: String) extends ProjectServiceFailure

}
