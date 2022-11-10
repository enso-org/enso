package org.enso.languageserver.vcsmanager

import java.io.{FileNotFoundException, IOException}
import java.nio.file.Path
import org.enso.languageserver.effect.BlockingIO
import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.api.errors.RefNotFoundException
import org.eclipse.jgit.api.errors.WrongRepositoryStateException
import org.eclipse.jgit.errors.{
  IncorrectObjectTypeException,
  InvalidObjectIdException,
  MissingObjectException,
  RepositoryNotFoundException
}
import org.eclipse.jgit.lib.{ObjectId, Repository}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.enso.languageserver.vcsmanager.Git.{
  AuthoerEmail,
  AuthorName,
  DefaultGitRepoDir,
  MasterRef
}

import scala.jdk.CollectionConverters._
import zio.blocking.effectBlocking

import java.time.Instant

class Git extends VcsApi[BlockingIO] {
  override def init(root: Path): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val rootFile = root.toFile
      if (!rootFile.exists()) {
        throw new FileNotFoundException("unable to find project repo: " + root)
      }
      val repoLocation = root.resolve(DefaultGitRepoDir)
      if (repoLocation.toFile.exists()) {
        throw new WrongRepositoryStateException("repository already exists")
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
        .setAuthor(AuthorName, AuthoerEmail)
        .call()
      ()
    }.mapError(errorHandling)
  }

  private def repository(path: Path): Repository = {
    val builder = new FileRepositoryBuilder();
    builder
      .setGitDir(path.resolve(Git.DefaultGitRepoDir).toFile)
      .setMustExist(true)
      .build()
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
        .setAuthor(AuthorName, AuthoerEmail)
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
      val resetCmd0 = new JGit(repo)
        .reset()
        .setMode(ResetType.HARD)

      val resetCmd = commitId match {
        case Some(name) =>
          val found = findRevision(repo, name)
          found.map { rev =>
            resetCmd0
              .setRef(rev.getName())
          } getOrElse {
            throw new RefNotFoundException(name)
          }
        case None =>
          resetCmd0
      }
      resetCmd.call()
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

  override def list(root: Path): BlockingIO[VcsFailure, List[RepoCommit]] = {
    effectBlocking {
      val jgit   = new JGit(repository(root))
      val logCmd = jgit.log()
      logCmd
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
    case ex                              => GenericVcsFailure(ex.getMessage)
  }
}

object Git {
  val DefaultGitRepoDir = ".git"
  val MasterRef         = "refs/heads/master"
  val AuthorName        = "Enso VCS"
  val AuthoerEmail      = "enso@vcs.io"
}
