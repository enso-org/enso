package org.enso.languageserver.vcsmanager

import zio.blocking.effectBlocking

import java.io.FileNotFoundException
import java.nio.file.{Path}

import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.api.ResetCommand.ResetType
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import scala.jdk.CollectionConverters._

import org.enso.languageserver.effect.BlockingIO

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
    name: String
  ): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val jgit = new JGit(repository(root))
      val addCmd =
        jgit
          .add()
          .addFilepattern(".")

      addCmd.call()

      val commitCmd =
        jgit
          .commit()
          .setMessage(name)
          .setAuthor("Enso VCS", "vcs@enso.io")
      commitCmd.call()
      ()
    }.mapError(errorHandling)
  }

  override def restore(root: Path): BlockingIO[VcsFailure, Unit] = {
    effectBlocking {
      val jgit = new JGit(repository(root))
      val resetCmd = jgit
        .reset()
        .setMode(ResetType.HARD)
      resetCmd.call()
      ()
    }.mapError(errorHandling)
  }

  override def modified(root: Path): BlockingIO[VcsFailure, Boolean] = {
    effectBlocking {
      val jgit      = new JGit(repository(root))
      val statusCmd = jgit.status()
      val status    = statusCmd.call()
      !status.isClean()
    }.mapError(errorHandling)
  }

  override def list(root: Path): BlockingIO[VcsFailure, List[String]] = {
    effectBlocking {
      val jgit   = new JGit(repository(root))
      val logCmd = jgit.log()
      logCmd.call().asScala.toList.map(_.getFullMessage())
    }.mapError(errorHandling)
  }

  private val errorHandling: Throwable => VcsFailure = {
    case ex: FileNotFoundException => ProjectNotFound(ex.getMessage)
    case ex                        => GenericVcsFailure(ex.getMessage)
  }
}

object Git {
  val DefaultGitRepoDir = ".git"
}
