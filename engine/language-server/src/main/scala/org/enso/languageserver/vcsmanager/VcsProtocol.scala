package org.enso.languageserver.vcsmanager

import org.enso.languageserver.filemanager.Path

object VcsProtocol {

  case class InitRepo(root: Path)

  case class InitRepoResult(result: Either[VcsFailure, Unit])

  case class CommitRepo(root: Path, name: Option[String])

  case class CommitRepoResult(result: Either[VcsFailure, Unit])

  case class RestoreRepo(root: Path, revName: Option[String])

  case class RestoreRepoResult(result: Either[VcsFailure, Unit])

  case class StatusRepo(root: Path)

  case class StatusRepoResult(
    result: Either[VcsFailure, (Boolean, List[Path], String)]
  )

  case class ListRepo(root: Path)

  case class ListRepoResult(result: Either[VcsFailure, List[String]])

}
