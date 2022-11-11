package org.enso.languageserver.vcsmanager

import java.nio.file.Path

abstract class VcsApi[F[_, _]] {

  /** Initialize VCS for the project. If the project is already under a version control, failure is reported.
    *
    * @param root top directory of the project
    * @return any failures during the initialization
    */
  def init(root: Path): F[VcsFailure, Unit]

  /** Commit any pending changes to the project.
    * Any new or changed files will be recorded.
    *
    * VCS commits are always committed with the timestamp in the message.
    *
    * @param root location of the project under version control
    * @param name an optional message to prefix the commit
    * @return any failures during the commit, or the metadata of the recorded commit
    */
  def commit(root: Path, name: Option[String]): F[VcsFailure, RepoCommit]

  /** Commit any pending changes to the project.
    * Any new or changed files will be recorded.
    *
    * VCS commits are always committed with the timestamp in the message.
    *
    * @param root location of the project under version control
    * @param name a message to prefix the commit
    * @return any failures during the commit, or the metadata of the recorded commit
    */
  def commit(root: Path, name: String): F[VcsFailure, RepoCommit] =
    commit(root, Some(name))

  /** Restore the state of the repository to some past commit.
    * Any changes that have not been recorded under VCS will be lost.
    *
    * If no commit is provided, VCS resets project by dropping any unrecorded changes.
    *
    * @param root location of the project under version control
    * @param commitId optional commit to which the project should be reverted to
    * @return any failures during the commit
    */
  def restore(root: Path, commitId: Option[String]): F[VcsFailure, Unit]

  /** Report the current status of the project, reporting all modified, new or deleted projects.
    *
    * @param root location of the project under version control
    * @return any failures during the status check, or the metadata of project status
    */
  def status(root: Path): F[VcsFailure, RepoStatus]

  /** List recorded commits in the descending order.
    *
    * @param root location of the project under version control
    * @param limit optional maximal number of recorded commits. If 0 returns all
    * @return any failures during the status check, or the list of requested commits' metadata
    */
  def list(
    root: Path,
    limit: Option[Int] = None
  ): F[VcsFailure, List[RepoCommit]]
}

/** `RepoStatus` encapsulates the current state of the project under VCS.
  *
  * @param isDirty `true`, if there are any unsaved changes to the project, `false` otherwise
  * @param changed list of modified, new or deleted files that have not yet been recorded permanently in VCS
  * @param lastCommit metadata of the last commit recorder for the project
  */
case class RepoStatus(
  isDirty: Boolean,
  changed: Set[Path],
  lastCommit: RepoCommit
)

/** Encapsulates metadata of a single commit of the project.
  *
  * @param commitId identifier, assigned by the underlying VCS, uniquely identifying the commit
  * @param message message associated with the commit
  */
case class RepoCommit(commitId: String, message: String)
