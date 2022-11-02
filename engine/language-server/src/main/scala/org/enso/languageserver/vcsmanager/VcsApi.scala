package org.enso.languageserver.vcsmanager

import java.nio.file.Path

abstract class VcsApi[F[_, _]] {

  def init(root: Path): F[VcsFailure, Unit]

  def commit(root: Path, name: String): F[VcsFailure, Unit]

  def restore(root: Path): F[VcsFailure, Unit]

  def modified(root: Path): F[VcsFailure, Boolean]

  def list(root: Path): F[VcsFailure, List[String]]

}
