package org.enso.languageserver

import akka.actor.{Actor, ActorLogging, Stash}
import cats.effect.IO
import org.enso.languageserver.data._
import org.enso.languageserver.event.{
  ClientConnected,
  ClientDisconnected,
  ClientEvent
}
import org.enso.languageserver.filemanager.FileManagerProtocol._
import org.enso.languageserver.filemanager.{
  DirectoryTree,
  FileSystemApi,
  FileSystemObject
}

object LanguageProtocol {

  /** Initializes the Language Server. */
  case object Initialize

}

/**
  * An actor representing an instance of the Language Server.
  *
  * @param config the configuration used by this Language Server.
  */
class LanguageServer(config: Config, fs: FileSystemApi[IO])
    extends Actor
    with Stash
    with ActorLogging {
  import LanguageProtocol._

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ClientEvent])
  }

  override def receive: Receive = {
    case Initialize =>
      log.debug("Language Server initialized.")
      unstashAll()
      context.become(initialized(config))
    case _ => stash()
  }

  def initialized(
    config: Config,
    env: Environment = Environment.empty
  ): Receive = {
    case ClientConnected(client) =>
      log.info("Client connected [{}].", client.id)
      context.become(
        initialized(config, env.addClient(client))
      )

    case ClientDisconnected(clientId) =>
      log.info("Client disconnected [{}].", clientId)
      context.become(initialized(config, env.removeClient(clientId)))

    case WriteFile(path, content) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.write(path.toFile(rootPath), content).unsafeRunSync()
        } yield ()

      sender ! WriteFileResult(result)

    case ReadFile(path) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          content  <- fs.read(path.toFile(rootPath)).unsafeRunSync()
        } yield content

      sender ! ReadFileResult(result)

    case CreateFile(FileSystemObject.File(name, path)) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.createFile(path.toFile(rootPath, name)).unsafeRunSync()
        } yield ()

      sender ! CreateFileResult(result)

    case CreateFile(FileSystemObject.Directory(name, path)) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.createDirectory(path.toFile(rootPath, name)).unsafeRunSync()
        } yield ()

      sender ! CreateFileResult(result)

    case DeleteFile(path) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.delete(path.toFile(rootPath)).unsafeRunSync()
        } yield ()

      sender ! DeleteFileResult(result)

    case CopyFile(from, to) =>
      val result =
        for {
          rootPathFrom <- config.findContentRoot(from.rootId)
          rootPathTo   <- config.findContentRoot(to.rootId)
          _ <- fs
            .copy(from.toFile(rootPathFrom), to.toFile(rootPathTo))
            .unsafeRunSync()
        } yield ()

      sender ! CopyFileResult(result)

    case MoveFile(from, to) =>
      val result =
        for {
          rootPathFrom <- config.findContentRoot(from.rootId)
          rootPathTo   <- config.findContentRoot(to.rootId)
          _ <- fs
            .move(from.toFile(rootPathFrom), to.toFile(rootPathTo))
            .unsafeRunSync()
        } yield ()

      sender ! MoveFileResult(result)

    case ExistsFile(path) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          exists   <- fs.exists(path.toFile(rootPath)).unsafeRunSync()
        } yield exists

      sender ! ExistsFileResult(result)

    case TreeFile(path, depth) =>
      val result =
        for {
          rootPath  <- config.findContentRoot(path.rootId)
          directory <- fs.tree(path.toFile(rootPath), depth).unsafeRunSync()
        } yield DirectoryTree.fromDirectoryEntry(rootPath, path, directory)

      sender ! TreeFileResult(result)
  }
  /* Note [Usage of unsafe methods]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     It invokes side-effecting function, all exceptions are caught and
     explicitly returned as left side of disjunction.
 */
}
