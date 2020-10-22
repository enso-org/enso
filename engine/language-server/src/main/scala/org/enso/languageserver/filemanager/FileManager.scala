package org.enso.languageserver.filemanager

import akka.actor.{Actor, ActorLogging, Props}
import akka.routing.SmallestMailboxPool
import akka.pattern.pipe
import org.enso.languageserver.effect._
import org.enso.languageserver.data.Config
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging
import zio._

/** Handles the [[FileManagerProtocol]] messages, executes the [[FileSystem]]
  * effects and forms the responses.
  *
  * @param config configuration
  * @param fs an instance of a [[FileSystem]] that creates the effects
  * @param exec effects executor
  */
class FileManager(
  config: Config,
  fs: FileSystemApi[BlockingIO],
  exec: Exec[BlockingIO]
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = {
    case Ping =>
      sender() ! Pong

    case FileManagerProtocol.GetContentRoots =>
      sender() ! FileManagerProtocol.ContentRootsResult(
        config.contentRoots.keySet
      )

    case FileManagerProtocol.WriteFile(path, content) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          _        <- fs.write(path.toFile(rootPath), content)
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.WriteFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.WriteBinaryFile(path, contents) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          _        <- fs.writeBinary(path.toFile(rootPath), contents)
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.WriteFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.ReadFile(path) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          file = path.toFile(rootPath)
          content <- fs.read(file)
        } yield FileManagerProtocol.TextualFileContent(file, content)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ReadTextualFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.ReadBinaryFile(path) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          file = path.toFile(rootPath)
          contents <- fs.readBinary(file)
        } yield FileManagerProtocol.BinaryFileContent(file, contents)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ReadBinaryFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.CreateFile(FileSystemObject.File(name, path)) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          _        <- fs.createFile(path.toFile(rootPath, name))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.CreateFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.CreateFile(
          FileSystemObject.Directory(name, path)
        ) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          _        <- fs.createDirectory(path.toFile(rootPath, name))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.CreateFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.DeleteFile(path) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          _        <- fs.delete(path.toFile(rootPath))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.DeleteFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.CopyFile(from, to) =>
      val result =
        for {
          rootPathFrom <- IO.fromEither(config.findContentRoot(from.rootId))
          rootPathTo   <- IO.fromEither(config.findContentRoot(to.rootId))
          _            <- fs.copy(from.toFile(rootPathFrom), to.toFile(rootPathTo))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.CopyFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.MoveFile(from, to) =>
      val result =
        for {
          rootPathFrom <- IO.fromEither(config.findContentRoot(from.rootId))
          rootPathTo   <- IO.fromEither(config.findContentRoot(to.rootId))
          _            <- fs.move(from.toFile(rootPathFrom), to.toFile(rootPathTo))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.MoveFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ExistsFile(path) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          exists   <- fs.exists(path.toFile(rootPath))
        } yield exists
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ExistsFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ListFile(path) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          entries  <- fs.list(path.toFile(rootPath))
        } yield entries.map(FileSystemObject.fromEntry(rootPath, path, _))
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ListFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.TreeFile(path, depth) =>
      val result =
        for {
          rootPath  <- IO.fromEither(config.findContentRoot(path.rootId))
          directory <- fs.tree(path.toFile(rootPath), depth)
        } yield DirectoryTree.fromDirectoryEntry(rootPath, path, directory)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.TreeFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.InfoFile(path) =>
      val result =
        for {
          rootPath <- IO.fromEither(config.findContentRoot(path.rootId))
          attrs    <- fs.info(path.toFile(rootPath))
        } yield FileAttributes.fromFileSystemAttributes(rootPath, path, attrs)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.InfoFileResult)
        .pipeTo(sender())
      ()
  }
}

object FileManager {

  def props(config: Config, fs: FileSystem, exec: Exec[BlockingIO]): Props =
    Props(new FileManager(config, fs, exec))

  def pool(config: Config, fs: FileSystem, exec: Exec[BlockingIO]): Props =
    SmallestMailboxPool(config.fileManager.parallelism)
      .props(props(config, fs, exec))
}
