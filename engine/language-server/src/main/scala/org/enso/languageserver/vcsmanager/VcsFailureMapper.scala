package org.enso.languageserver.vcsmanager

import org.enso.jsonrpc.Error

object VcsFailureMapper {

  def mapFailure(failure: VcsFailure): Error = failure match {
    case ProjectNotFound(_)     => VcsManagerApi.ProjectNotFound
    case RepoNotFound(_)        => VcsManagerApi.RepoNotFound
    case ProjectRootNotFound(_) => VcsManagerApi.ContentRootNotFoundError
    case SaveNotFound           => VcsManagerApi.NamedSaveNotFound
    case GenericVcsFailure(msg) => VcsManagerApi.VcsError(msg)
  }

}
