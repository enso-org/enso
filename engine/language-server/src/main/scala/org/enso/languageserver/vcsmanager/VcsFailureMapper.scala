package org.enso.languageserver.vcsmanager

import org.enso.jsonrpc.Error

object VcsFailureMapper {

  def mapFailure(failure: VcsFailure): Error = failure match {
    case ProjectNotFound(_)     => VcsManagerApi.ProjectNotFound
    case ProjectRootNotFound(_) => VcsManagerApi.ContentRootNotFoundError
    case GenericVcsFailure(msg) => VcsManagerApi.VcsError(msg)
  }

}
