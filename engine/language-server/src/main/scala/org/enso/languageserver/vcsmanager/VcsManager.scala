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

  private def toVcsError: FileSystemFailure => VcsFailure = {
    ex: FileSystemFailure => ProjectNotFound(ex.toString)
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
        .map(VcsProtocol.InitRepoResponse)
        .pipeTo(sender())
    case VcsProtocol.SaveRepo(_, repoRoot, name) =>
      val result =
        for {
          root      <- resolvePath(repoRoot)
          revCommit <- vcs.commit(root.toPath, name)
        } yield revCommit
      exec
        .execTimed(config.timeout, result)
        .map(r =>
          VcsProtocol.SaveRepoResponse(r.map(RepoCommit.unapply(_).get))
        )
        .pipeTo(sender())
    case VcsProtocol.RestoreRepo(_, repoRoot, optRevName) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          _    <- vcs.restore(root.toPath, optRevName)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(VcsProtocol.RestoreRepoResponse)
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
          VcsProtocol.StatusRepoResponse(
            r.map(status =>
              (
                status.isDirty,
                status.changed.map(f => Path(repoRoot.rootId, f)).toList,
                RepoCommit.unapply(status.lastCommit).get
              )
            )
          )
        )
        .pipeTo(sender())
    case VcsProtocol.ListRepo(_, repoRoot, limit) =>
      val result =
        for {
          root <- resolvePath(repoRoot)
          tags <- vcs.list(root.toPath, limit)
        } yield tags
      exec
        .execTimed(config.timeout, result)
        .map(r =>
          VcsProtocol.ListRepoResponse(r.map(_.map(RepoCommit.unapply(_).get)))
        )
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
