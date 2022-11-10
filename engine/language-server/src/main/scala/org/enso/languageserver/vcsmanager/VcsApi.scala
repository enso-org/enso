package org.enso.languageserver.vcsmanager

import java.nio.file.Path

abstract class VcsApi[F[_, _]] {

  def init(root: Path): F[VcsFailure, Unit]

  def commit(root: Path, name: Option[String]): F[VcsFailure, RepoCommit]

  def commit(root: Path, name: String): F[VcsFailure, RepoCommit] =
    commit(root, Some(name))

  def restore(root: Path, commitId: Option[String]): F[VcsFailure, Unit]

  def status(root: Path): F[VcsFailure, RepoStatus]

  def list(root: Path): F[VcsFailure, List[RepoCommit]]

}

case class RepoStatus(
  isDirty: Boolean,
  changed: Set[Path],
  lastCommit: RepoCommit
)

case class RepoCommit(commitId: String, message: String)
