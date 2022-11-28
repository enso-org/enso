package org.enso.languageserver.vcsmanager

import java.io.{FileNotFoundException, IOException}
import java.nio.file.Path
import org.enso.languageserver.effect.BlockingIO
import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.api.errors.RefNotFoundException
import org.eclipse.jgit.errors.{
  IncorrectObjectTypeException,
  InvalidObjectIdException,
  MissingObjectException,
  RepositoryNotFoundException
}
import org.eclipse.jgit.lib.{ObjectId, Repository}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.util.SystemReader
import org.enso.languageserver.vcsmanager.Git.{
  AuthorEmail,
  AuthorName,
  DefaultGitRepoDir,
  MasterRef,
  RepoExists
}

import scala.jdk.CollectionConverters._
import zio.blocking.effectBlocking

import java.time.Instant

private class Git extends VcsApi[BlockingIO] {

  private def repository(path: Path): Repository = {
    val builder = new FileRepositoryBuilder()
    builder
      .setGitDir(path.resolve(Git.DefaultGitRepoDir).toFile)
      .setMustExist(true)
      .build()
  }

  override def init(root: Path): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val rootFile = root.toFile
      if (!rootFile.exists()) {
        throw new FileNotFoundException("unable to find project repo: " + root)
      }
      val repoLocation = root.resolve(DefaultGitRepoDir)
      if (repoLocation.toFile.exists()) {
        throw new RepoExists()
      }

      val jgit = JGit
        .init()
        .setDirectory(rootFile)
        .setBare(false)
        .call()

      jgit
        .commit()
        .setAllowEmpty(true)
        .setAll(true)
        .setMessage("Initial commit")
        .setAuthor(AuthorName, AuthorEmail)
        .call()
      ()
    }.mapError(errorHandling)
  }

  override def commit(
    root: Path,
    named: Option[String]
  ): BlockingIO[VcsFailure, RepoCommit] = {
    effectBlocking {
      val repo = repository(root)

      val commitName = named.getOrElse(Instant.now().toString)
      val jgit       = new JGit(repo)
      // Add all modified and new files to the index
      jgit
        .add()
        .addFilepattern(".")
        .call()

      val revCommit = jgit
        .commit()
        .setMessage(commitName)
        .setAuthor(AuthorName, AuthorEmail)
        .call()
      RepoCommit(revCommit.getName(), revCommit.getShortMessage())
    }.mapError(errorHandling)
  }

  override def restore(
    root: Path,
    commitId: Option[String]
  ): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val repo = repository(root)

      val jgit = new JGit(repo)
      val resetCmd = jgit
        .reset()
        .setMode(ResetType.HARD)

      commitId match {
        case Some(name) =>
          val foundRev = findRevision(repo, name).getOrElse(
            throw new RefNotFoundException(name)
          )
          // Reset first to avoid checkout conflicts
          resetCmd.call()
          jgit
            .checkout()
            .setName(foundRev.getName)
            .call()
        case None =>
          resetCmd.call()
      }
      ()
    }.mapError(errorHandling)
  }

  private def findRevision(
    repo: Repository,
    sha: String
  ): Option[RevCommit] = {
    val walk = new RevWalk(repo)
    try {
      val revCommitId = ObjectId.fromString(sha)
      Some(walk.parseCommit(revCommitId))
    } catch {
      case _: MissingObjectException       => None
      case _: IncorrectObjectTypeException => None
      case _: InvalidObjectIdException     => None
      case _: IOException                  => None
    } finally {
      walk.dispose()
    }
  }

  override def status(root: Path): BlockingIO[VcsFailure, RepoStatus] = {
    effectBlocking {
      val repo      = repository(root)
      val jgit      = new JGit(repo)
      val statusCmd = jgit.status()
      val status    = statusCmd.call()
      val changed =
        status.getModified().asScala.toList ++
        status.getUntracked().asScala.toList ++
        status.getRemoved().asScala.toList
      val changedPaths = changed.map(name => Path.of(name)).toSet
      val logCmd       = jgit.log()
      val last =
        Option(repo.resolve(MasterRef)) flatMap (_ => {
          val logs = logCmd.setMaxCount(1).call().iterator()
          if (logs.hasNext()) {
            val log = logs.next()
            Option(RepoCommit(log.getName, log.getShortMessage()))
          } else None
        }) getOrElse null
      RepoStatus(!status.isClean(), changedPaths, last)
    }.mapError(errorHandling)
  }

  override def list(
    root: Path,
    limit: Option[Int]
  ): BlockingIO[VcsFailure, List[RepoCommit]] = {
    effectBlocking {
      val jgit   = new JGit(repository(root))
      val logCmd = jgit.log()
      limit
        .filter(_ > 0)
        .map(logCmd.setMaxCount)
        .getOrElse(logCmd)
        .call()
        .asScala
        .toList
        .map(rev => RepoCommit(rev.getName, rev.getShortMessage))
    }.mapError(errorHandling)
  }

  private val errorHandling: Throwable => VcsFailure = {
    case ex: FileNotFoundException       => ProjectNotFound(ex.getMessage)
    case ex: RepositoryNotFoundException => RepoNotFound(ex.getMessage)
    case _: RefNotFoundException         => SaveNotFound
    case _: RepoExists                   => RepoAlreadyExists
    case ex                              => GenericVcsFailure(ex.getMessage)
  }
}

object Git {
  private val DefaultGitRepoDir = ".git"
  private val MasterRef         = "refs/heads/master"
  private val AuthorName        = "Enso VCS"
  private val AuthorEmail       = "vcs@enso.io"

  private class RepoExists extends Exception

  /** Returns a Git implementation of VcsApi that ignores gitconfig file in
    * user's home directory.
    */
  def withEmptyUserConfig(): VcsApi[BlockingIO] = {
    SystemReader.setInstance(new EmptyUserConfigReader)
    new Git()
  }
}
