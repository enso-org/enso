package org.enso.languageserver.vcsmanager

import java.io.FileNotFoundException
import java.nio.file.Path
import org.enso.languageserver.effect.BlockingIO
import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.api.errors.RefAlreadyExistsException
import org.eclipse.jgit.api.errors.RefNotFoundException
import org.eclipse.jgit.errors.RepositoryNotFoundException
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.enso.languageserver.vcsmanager.Git.MasterRef

import scala.jdk.CollectionConverters._
import zio.blocking.effectBlocking

import java.time.Instant
import scala.annotation.tailrec

class Git extends VcsApi[BlockingIO] {
  override def init(root: Path): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val rootFile = root.toFile
      if (!rootFile.exists()) {
        throw new FileNotFoundException("unable to find project repo: " + root)
      }
      val initCmd =
        JGit
          .init()
          .setDirectory(rootFile)
          .setBare(false)
      val jgit = initCmd.call()
      val commitCmd =
        jgit
          .commit()
          .setAllowEmpty(true)
          .setAll(true)
          .setMessage("Initial commit")
          .setAuthor("Enso VCS", "vcs@enso.io")
      commitCmd.call()
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
  ): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val repo = repository(root)
      val jgit = new JGit(repo)
      val addCmd =
        jgit
          .add()
          .addFilepattern(".")

      addCmd.call()

      // Ensure that the name is unique
      val commitName = named match {
        case Some(name) =>
          val walk = new RevWalk(repo)
          val from = repo.resolve(Git.MasterRef)
          walk.markStart(walk.parseCommit(from))
          val found = try {
            findRevision(walk, name)
          } finally {
            walk.dispose()
          }
          if (found.isEmpty) name
          else {
            throw new RefAlreadyExistsException(name + " already exists")
          }
        case None =>
          Instant.now().toString
      }

      jgit
        .commit()
        .setMessage(commitName)
        .setAuthor("Enso VCS", "vcs@enso.io")
        .call()
      ()
    }.mapError(errorHandling)
  }

  override def restore(root: Path, name: Option[String]): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val repo = repository(root)
      name match {
        case Some(name) =>
          val walk = new RevWalk(repo)
          val from = repo.resolve(MasterRef)
          walk.markStart(walk.parseCommit(from))
          val found = try {
            findRevision(walk, name)
          } finally {
            walk.dispose()
          }
          if (found.isEmpty) {
            throw new RefNotFoundException(name)
          } else {
            val jgit = new JGit(repo)
            jgit
              .reset()
              .setMode(ResetType.HARD)
              .setRef(found.get.getName())
              .call()
          }
        case None =>
          val jgit = new JGit(repo)
          val resetCmd = jgit
            .reset()
            .setMode(ResetType.HARD)
          resetCmd.call()
      }
      ()
    }.mapError(errorHandling)
  }

  private def findRevision(walk: RevWalk, name: String): Option[RevCommit] = {
    @tailrec
    def find(current: RevCommit, it: java.util.Iterator[RevCommit]): Option[RevCommit] = {
      if (current == null) None
      else if (current.getShortMessage() == name) Some(current)
      else find(it.next(), it)
    }
    val iterator = walk.iterator()
    find(iterator.next(), iterator)
  }

  override def status(root: Path): BlockingIO[VcsFailure, RepoStatus] = {
    effectBlocking {
      val jgit      = new JGit(repository(root))
      val statusCmd = jgit.status()
      val status    = statusCmd.call()
      val changed = status.getModified().asScala.toList ++ status
        .getUntracked()
        .asScala
        .toList ++ status.getRemoved().asScala.toList
      val changedPaths = changed.map(name => Path.of(name)).toSet
      val logCmd       = jgit.log()
      val last         = logCmd.setMaxCount(1).call().iterator().next().getShortMessage()
      RepoStatus(!status.isClean(), changedPaths, last)
    }.mapError(errorHandling)
  }

  override def list(root: Path): BlockingIO[VcsFailure, List[String]] = {
    effectBlocking {
      val jgit   = new JGit(repository(root))
      val logCmd = jgit.log()
      logCmd.call().asScala.toList.map(_.getShortMessage())
    }.mapError(errorHandling)
  }

  private val errorHandling: Throwable => VcsFailure = {
    case ex: FileNotFoundException       => ProjectNotFound(ex.getMessage)
    case ex: RepositoryNotFoundException => RepoNotFound(ex.getMessage)
    case _ : RefAlreadyExistsException => SaveAlreadyExists
    case _: RefNotFoundException => SaveNotFound
    case ex =>
      ex.printStackTrace()
      GenericVcsFailure(ex.getMessage)
  }
}

object Git {
  val DefaultGitRepoDir = ".git"
  val MasterRef = "refs/heads/master"
}
