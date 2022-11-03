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
    case VcsProtocol.InitRepo(_, repoRoot) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          _    <- vcs.init(root.toPath)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.InitRepoResult)
        .pipeTo(sender())
    case VcsProtocol.SaveRepo(_, repoRoot, name) =>
      val result =
        for {
          root      <- resolvePath(repoRoot)
          revCommit <- vcs.commit(root.toPath, name)
        } yield revCommit
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.SaveRepoResult)
        .pipeTo(sender())
    case VcsProtocol.RestoreRepo(_, repoRoot, optRevName) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          _    <- vcs.restore(root.toPath, optRevName)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.RestoreRepoResult)
        .pipeTo(sender())
    case VcsProtocol.StatusRepo(_, repoRoot) =>
      val result =
        for {
          root   <- resolvePath(repoRoot)
          status <- vcs.status(root.toPath)
        } yield status
      exec
        .execTimed(config.timeout, result)
        .map(r =>
          VcsProtocol.StatusRepoResult(
            r.map(status =>
              (
                status.isDirty,
                status.changed.map(f => Path(repoRoot.rootId, f)).toList,
                status.lastCommit
              )
            )
          )
        )
        .pipeTo(sender())
    case VcsProtocol.ListRepo(_, repoRoot) =>
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
