package org.enso.projectmanager.service

import nl.gn0s1s.bump.SemVer

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

  /** Signals that a failure occured when creating the project.
    *
    * @param message a failure message
    */
  case class ProjectCreateFailed(message: String) extends ProjectServiceFailure

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

  /** Signals that a component required to complete the action was missing, but
    * the action did not ask for it to be automatically installed.
    */
  case class MissingComponentFailure(msg: String) extends ProjectServiceFailure

  /** Signals that a component that was being installed is marked as broken, but
    * the option to forcibly install broken components was not set.
    */
  case class BrokenComponentFailure(msg: String) extends ProjectServiceFailure

  /** Signals that installation of a missing compoment has been attempted, but
    * the required engine version requires a newer version of project manager
    * than what is currently running.
    */
  case class ProjectManagerUpgradeRequiredFailure(
    minimumRequiredVersion: SemVer
  ) extends ProjectServiceFailure

  /** Signals that installation of a missing component has been attempted but it
    * has failed.
    */
  case class ComponentInstallationFailure(msg: String)
      extends ProjectServiceFailure

  /** Signals that uninstallation of a component has failed. */
  case class ComponentUninstallationFailure(msg: String)
      extends ProjectServiceFailure

  /** Signals that the version repository is unavailable and could not be
    * queried (usually caused by lack of internet connection).
    */
  case class ComponentRepositoryAccessFailure(msg: String)
      extends ProjectServiceFailure
}
