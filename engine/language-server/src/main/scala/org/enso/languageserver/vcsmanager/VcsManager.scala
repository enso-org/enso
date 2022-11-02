package org.enso.languageserver.vcsmanager

import akka.actor.{Actor, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.VcsManagerConfig
import org.enso.languageserver.effect.{BlockingIO, Exec}
import org.enso.languageserver.filemanager.{
  ContentRootManager,
  ContentRootNotFound,
  ContentRootWithFile,
  FileSystemFailure,
  Path
}
import org.enso.languageserver.util.UnhandledLogging
import zio.IO

import java.io.File
import java.util.UUID

class VcsManager(
  config: VcsManagerConfig,
  vcs: VcsApi[BlockingIO],
  contentRootManager: ContentRootManager,
  exec: Exec[BlockingIO]
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  private def findContentRoot(
    id: UUID
  ): IO[FileSystemFailure, ContentRootWithFile] =
    IO.fromFuture { ec => contentRootManager.findContentRoot(id)(ec) }
      .mapError { _ => ContentRootNotFound }
      .absolve

  // TODO: better conversion
  private def toVcsError: FileSystemFailure => VcsFailure = {
    case ex: FileSystemFailure => ProjectRootNotFound(ex.toString)
  }

  private def resolvePath(path: Path): IO[VcsFailure, File] =
    findContentRoot(path.rootId)
      .map { root => path.toFile(root.file) }
      .mapError(toVcsError)

  override def receive: Receive = {
    case VcsProtocol.InitRepo(repoRoot) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          _    <- vcs.init(root.toPath)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.InitRepoResult)
        .pipeTo(sender())
    case VcsProtocol.CommitRepo(repoRoot, name) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          _    <- vcs.commit(root.toPath, name)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.CommitRepoResult)
        .pipeTo(sender())
    case VcsProtocol.RestoreRepo(repoRoot) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          _    <- vcs.restore(root.toPath)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.RestoreRepoResult)
        .pipeTo(sender())
    case VcsProtocol.ModifiedRepo(repoRoot) =>
      val result =
        for {
          root   <- resolvePath(repoRoot)
          status <- vcs.modified(root.toPath)
        } yield status
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.ModifiedRepoResult)
        .pipeTo(sender())
    case VcsProtocol.ListRepo(repoRoot) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          tags <- vcs.list(root.toPath)
        } yield tags
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.ListRepoResult)
        .pipeTo(sender())
  }
}

object VcsManager {

  def props(
    config: VcsManagerConfig,
    vcs: VcsApi[BlockingIO],
    contentRootManager: ContentRootManager,
    exec: Exec[BlockingIO]
  ): Props =
    Props(new VcsManager(config, vcs, contentRootManager, exec))

}
