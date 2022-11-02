package org.enso.languageserver.vcsmanager

import org.enso.languageserver.filemanager.Path

object VcsProtocol {

  case class InitRepo(root: Path)

  case class InitRepoResult(result: Either[VcsFailure, Unit])

  case class CommitRepo(root: Path, name: String)

  case class CommitRepoResult(result: Either[VcsFailure, Unit])

  case class RestoreRepo(root: Path)

  case class RestoreRepoResult(result: Either[VcsFailure, Unit])

  case class ModifiedRepo(root: Path)

  case class ModifiedRepoResult(result: Either[VcsFailure, Boolean])

  case class ListRepo(root: Path)

  case class ListRepoResult(result: Either[VcsFailure, List[String]])

}
