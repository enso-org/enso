package org.enso.languageserver.vcsmanager

sealed trait VcsFailure

case class RepoNotFound(reason: String) extends VcsFailure

case object RepoAlreadyExists extends VcsFailure

case class ProjectNotFound(reason: String) extends VcsFailure

case object SaveNotFound extends VcsFailure

case class GenericVcsFailure(reason: String) extends VcsFailure
