package org.enso.languageserver.vcsmanager

import org.enso.languageserver.data.ClientId
import org.enso.languageserver.filemanager.Path

object VcsProtocol {

  sealed trait VCSResponse[T] {
    def result: Either[VcsFailure, T]
  }

  case class InitRepo(clientId: ClientId, root: Path)

  case class InitRepoResponse(result: Either[VcsFailure, Unit])
      extends VCSResponse[Unit]

  case class SaveRepo(clientId: ClientId, root: Path, name: Option[String])

  case class SaveRepoResponse(result: Either[VcsFailure, (String, String)])
      extends VCSResponse[(String, String)]

  case class RestoreRepo(
    clientId: ClientId,
    root: Path,
    revName: Option[String]
  )

  case class RestoreRepoResponse(result: Either[VcsFailure, List[Path]])
      extends VCSResponse[List[Path]]

  case class StatusRepo(clientId: ClientId, root: Path)

  case class StatusRepoResponse(
    result: Either[VcsFailure, (Boolean, List[Path], Option[(String, String)])]
  ) extends VCSResponse[(Boolean, List[Path], Option[(String, String)])]

  case class ListRepo(clientId: ClientId, root: Path, limit: Option[Int])

  case class ListRepoResponse(
    result: Either[VcsFailure, List[(String, String)]]
  ) extends VCSResponse[List[(String, String)]]

}
