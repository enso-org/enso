package org.enso.languageserver.vcsmanager

import org.enso.jsonrpc.Error

object VcsFailureMapper {

  def mapFailure(failure: VcsFailure): Error = failure match {
    case ProjectNotFound(_)     => VcsManagerApi.ProjectNotFound
    case RepoNotFound(_)        => VcsManagerApi.VCSNotFound
    case RepoAlreadyExists      => VcsManagerApi.VCSAlreadyExists
    case SaveNotFound           => VcsManagerApi.SaveNotFound
    case GenericVcsFailure(msg) => VcsManagerApi.VCSError(msg)
  }

}
