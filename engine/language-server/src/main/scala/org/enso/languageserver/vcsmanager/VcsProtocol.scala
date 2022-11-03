package org.enso.languageserver.vcsmanager

import org.enso.languageserver.data.ClientId
import org.enso.languageserver.filemanager.Path

object VcsProtocol {

  case class InitRepo(clientId: ClientId, root: Path)

  case class InitRepoResult(result: Either[VcsFailure, Unit])

  case class SaveRepo(clientId: ClientId, root: Path, name: Option[String])

  case class SaveRepoResult(result: Either[VcsFailure, (String, String)])

  case class RestoreRepo(
    clientId: ClientId,
    root: Path,
    revName: Option[String]
  )

  case class RestoreRepoResult(result: Either[VcsFailure, Unit])

  case class StatusRepo(clientId: ClientId, root: Path)

  case class StatusRepoResult(
    result: Either[VcsFailure, (Boolean, List[Path], String)]
  )

  case class ListRepo(clientId: ClientId, root: Path)

  case class ListRepoResult(result: Either[VcsFailure, List[String]])

}
