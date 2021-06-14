package org.enso.languageserver.filemanager

import akka.actor.{Actor, Props}
import akka.routing.SmallestMailboxPool
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.bouncycastle.util.encoders.Hex
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
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = {
    case Ping =>
      sender() ! Pong

    case FileManagerProtocol.GetContentRoots =>
      sender() ! FileManagerProtocol.ContentRootsResult(
        config.contentRoots.values.map(_.toContentRoot).toSet
      )

    case FileManagerProtocol.WriteFile(path, content) =>
      val result =
        for {
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          _    <- fs.write(path.toFile(root.file), content)
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.WriteFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.WriteBinaryFile(path, contents) =>
      val result =
        for {
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          _    <- fs.writeBinary(path.toFile(root.file), contents)
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.WriteFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.ReadFile(path) =>
      val result =
        for {
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          file = path.toFile(root.file)
          content <- fs.read(file)
        } yield FileManagerProtocol.TextualFileContent(file, content)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ReadTextualFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.ReadBinaryFile(path) =>
      val result =
        for {
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          file = path.toFile(root.file)
          contents <- fs.readBinary(file)
        } yield FileManagerProtocol.BinaryFileContent(file, contents)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ReadBinaryFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.CreateFile(FileSystemObject.File(name, path)) =>
      val result =
        for {
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          _    <- fs.createFile(path.toFile(root.file, name))
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
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          _    <- fs.createDirectory(path.toFile(root.file, name))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.CreateFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.DeleteFile(path) =>
      val result =
        for {
          root <- IO.fromEither(config.findContentRoot(path.rootId))
          _    <- fs.delete(path.toFile(root.file))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.DeleteFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.CopyFile(from, to) =>
      val result =
        for {
          rootFrom <- IO.fromEither(config.findContentRoot(from.rootId))
          rootTo   <- IO.fromEither(config.findContentRoot(to.rootId))
          _        <- fs.copy(from.toFile(rootFrom.file), to.toFile(rootTo.file))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.CopyFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.MoveFile(from, to) =>
      val result =
        for {
          rootFrom <- IO.fromEither(config.findContentRoot(from.rootId))
          rootTo   <- IO.fromEither(config.findContentRoot(to.rootId))
          _        <- fs.move(from.toFile(rootFrom.file), to.toFile(rootTo.file))
        } yield ()
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.MoveFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ExistsFile(path) =>
      val result =
        for {
          root   <- IO.fromEither(config.findContentRoot(path.rootId))
          exists <- fs.exists(path.toFile(root.file))
        } yield exists
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ExistsFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ListFile(path) =>
      val result =
        for {
          root    <- IO.fromEither(config.findContentRoot(path.rootId))
          entries <- fs.list(path.toFile(root.file))
        } yield entries.map(FileSystemObject.fromEntry(root.file, path, _))
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.ListFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.TreeFile(path, depth) =>
      val result =
        for {
          root      <- IO.fromEither(config.findContentRoot(path.rootId))
          directory <- fs.tree(path.toFile(root.file), depth)
        } yield DirectoryTree.fromDirectoryEntry(root.file, path, directory)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.TreeFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.InfoFile(path) =>
      val result =
        for {
          root  <- IO.fromEither(config.findContentRoot(path.rootId))
          attrs <- fs.info(path.toFile(root.file))
        } yield FileAttributes.fromFileSystemAttributes(root.file, path, attrs)
      exec
        .execTimed(config.fileManager.timeout, result)
        .map(FileManagerProtocol.InfoFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ChecksumFileRequest(path) =>
      val getChecksum = for {
        root     <- IO.fromEither(config.findContentRoot(path.rootId))
        checksum <- fs.digest(path.toFile(root.file))
      } yield checksum
      exec
        .execTimed(config.fileManager.timeout, getChecksum)
        .map(x =>
          FileManagerProtocol.ChecksumFileResponse(
            x.map(digest => Hex.toHexString(digest.bytes))
          )
        )
        .pipeTo(sender())

    case FileManagerProtocol.ChecksumBytesRequest(segment) =>
      val getChecksum = for {
        root     <- IO.fromEither(config.findContentRoot(segment.path.rootId))
        checksum <- fs.digestBytes(segment.toApiSegment(root.file))
      } yield checksum
      exec
        .execTimed(config.fileManager.timeout, getChecksum)
        .map(x => FileManagerProtocol.ChecksumBytesResponse(x.map(_.bytes)))
        .pipeTo(sender())

    case FileManagerProtocol.WriteBytesRequest(path, off, overwrite, bytes) =>
      val doWrite = for {
        root     <- IO.fromEither(config.findContentRoot(path.rootId))
        response <- fs.writeBytes(path.toFile(root.file), off, overwrite, bytes)
      } yield response
      exec
        .execTimed(config.fileManager.timeout, doWrite)
        .map(x => FileManagerProtocol.WriteBytesResponse(x.map(_.bytes)))
        .pipeTo(sender())

    case FileManagerProtocol.ReadBytesRequest(segment) =>
      val doRead = for {
        root     <- IO.fromEither(config.findContentRoot(segment.path.rootId))
        response <- fs.readBytes(segment.toApiSegment(root.file))
      } yield response
      exec
        .execTimed(config.fileManager.timeout, doRead)
        .map(FileManagerProtocol.ReadBytesResponse)
        .pipeTo(sender())
  }
}

object FileManager {

  def props(config: Config, fs: FileSystem, exec: Exec[BlockingIO]): Props =
    Props(new FileManager(config, fs, exec))

  def pool(config: Config, fs: FileSystem, exec: Exec[BlockingIO]): Props =
    SmallestMailboxPool(config.fileManager.parallelism)
      .props(props(config, fs, exec))
}
