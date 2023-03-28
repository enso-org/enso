package org.enso.languageserver.filemanager

import akka.actor.{Actor, Props}
import akka.pattern.pipe
import akka.routing.SmallestMailboxPool
import com.typesafe.scalalogging.LazyLogging
import org.bouncycastle.util.encoders.Hex
import org.enso.languageserver.data.FileManagerConfig
import org.enso.languageserver.effect._
import org.enso.languageserver.filemanager.FileManagerProtocol.TextualFileContent
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging
import zio._

import java.io.File
import java.util.UUID

/** Handles the [[FileManagerProtocol]] messages, executes the [[FileSystem]]
  * effects and forms the responses.
  *
  * @param config configuration
  * @param contentRootManager the content root manager
  * @param fs an instance of a [[FileSystem]] that creates the effects
  * @param exec effects executor
  */
class FileManager(
  config: FileManagerConfig,
  contentRootManager: ContentRootManager,
  fs: FileSystemApi[BlockingIO],
  exec: Exec[BlockingIO]
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  private def findContentRoot(
    id: UUID
  ): IO[FileSystemFailure, ContentRootWithFile] =
    ZIO
      .fromFuture { ec => contentRootManager.findContentRoot(id)(ec) }
      .mapError { _ => ContentRootNotFound }
      .absolve

  private def resolvePath(path: Path): IO[FileSystemFailure, File] =
    findContentRoot(path.rootId).map { root => path.toFile(root.file) }

  override def receive: Receive = {
    case Ping =>
      sender() ! Pong

    case FileManagerProtocol.WriteFile(path, content) =>
      val result =
        for {
          file <- resolvePath(path)
          _    <- fs.write(file, content)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.WriteFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.WriteBinaryFile(path, contents) =>
      val result =
        for {
          file <- resolvePath(path)
          _    <- fs.writeBinary(file, contents)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.WriteFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.OpenBuffer(path) =>
      val result =
        for {
          file   <- resolvePath(path)
          exists <- fs.exists(file)
        } yield (file, exists)
      exec
        .execTimed(config.timeout, result)
        .map { result =>
          FileManagerProtocol.ReadTextualFileResult(
            result.flatMap { case (file, exists) =>
              if (exists) Left(FileExists)
              else Right(TextualFileContent(file, ""))
            }
          )
        }
        .pipeTo(sender())

    case FileManagerProtocol.ReadFile(path) =>
      val result =
        for {
          file    <- resolvePath(path)
          content <- fs.read(file)
        } yield FileManagerProtocol.TextualFileContent(file, content)
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.ReadTextualFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.ReadBinaryFile(path) =>
      val result =
        for {
          file     <- resolvePath(path)
          contents <- fs.readBinary(file)
        } yield FileManagerProtocol.BinaryFileContent(file, contents)
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.ReadBinaryFileResult)
        .pipeTo(sender())

    case FileManagerProtocol.CreateFile(FileSystemObject.File(name, path)) =>
      val result =
        for {
          root <- findContentRoot(path.rootId)
          _    <- fs.createFile(path.toFileInsideThisDirectory(root.file, name))
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.CreateFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.CreateFile(
          FileSystemObject.Directory(name, path)
        ) =>
      val result =
        for {
          root <- findContentRoot(path.rootId)
          _ <- fs.createDirectory(
            path.toFileInsideThisDirectory(root.file, name)
          )
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.CreateFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.DeleteFile(path) =>
      val result =
        for {
          file <- resolvePath(path)
          _    <- fs.delete(file)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.DeleteFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.CopyFile(from, to) =>
      val result =
        for {
          fileFrom <- resolvePath(from)
          fileTo   <- resolvePath(to)
          _        <- fs.copy(fileFrom, fileTo)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.CopyFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.MoveFile(from, to) =>
      val result =
        for {
          fileFrom <- resolvePath(from)
          fileTo   <- resolvePath(to)
          _        <- fs.move(fileFrom, fileTo)
        } yield ()
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.MoveFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ExistsFile(path) =>
      val result =
        for {
          file   <- resolvePath(path)
          exists <- fs.exists(file)
        } yield exists
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.ExistsFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ListFile(path) =>
      val result =
        for {
          root    <- findContentRoot(path.rootId)
          entries <- fs.list(path.toFile(root.file))
        } yield entries.map(FileSystemObject.fromEntry(root.file, path, _))
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.ListFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.TreeFile(path, depth) =>
      val result =
        for {
          root      <- findContentRoot(path.rootId)
          directory <- fs.tree(path.toFile(root.file), depth)
        } yield DirectoryTree.fromDirectoryEntry(root.file, path, directory)
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.TreeFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.InfoFile(path) =>
      val result =
        for {
          root  <- findContentRoot(path.rootId)
          attrs <- fs.info(path.toFile(root.file))
        } yield FileAttributes.fromFileSystemAttributes(root.file, path, attrs)
      exec
        .execTimed(config.timeout, result)
        .map(FileManagerProtocol.InfoFileResult)
        .pipeTo(sender())
      ()

    case FileManagerProtocol.ChecksumFileRequest(path) =>
      val getChecksum = for {
        file     <- resolvePath(path)
        checksum <- fs.digest(file)
      } yield checksum
      exec
        .execTimed(config.timeout, getChecksum)
        .map(x =>
          FileManagerProtocol.ChecksumFileResponse(
            x.map(digest => Hex.toHexString(digest.bytes))
          )
        )
        .pipeTo(sender())

    case FileManagerProtocol.ChecksumBytesRequest(segment) =>
      val getChecksum = for {
        root     <- findContentRoot(segment.path.rootId)
        checksum <- fs.digestBytes(segment.toApiSegment(root.file))
      } yield checksum
      exec
        .execTimed(config.timeout, getChecksum)
        .map(x => FileManagerProtocol.ChecksumBytesResponse(x.map(_.bytes)))
        .pipeTo(sender())

    case FileManagerProtocol.WriteBytesRequest(path, off, overwrite, bytes) =>
      val doWrite = for {
        file     <- resolvePath(path)
        response <- fs.writeBytes(file, off, overwrite, bytes)
      } yield response
      exec
        .execTimed(config.timeout, doWrite)
        .map(x => FileManagerProtocol.WriteBytesResponse(x.map(_.bytes)))
        .pipeTo(sender())

    case FileManagerProtocol.ReadBytesRequest(segment) =>
      val doRead = for {
        root     <- findContentRoot(segment.path.rootId)
        response <- fs.readBytes(segment.toApiSegment(root.file))
      } yield response
      exec
        .execTimed(config.timeout, doRead)
        .map(FileManagerProtocol.ReadBytesResponse)
        .pipeTo(sender())
  }
}

object FileManager {

  def props(
    config: FileManagerConfig,
    contentRootManager: ContentRootManager,
    fs: FileSystem,
    exec: Exec[BlockingIO]
  ): Props =
    Props(new FileManager(config, contentRootManager, fs, exec))

  def pool(
    config: FileManagerConfig,
    contentRootManager: ContentRootManager,
    fs: FileSystem,
    exec: Exec[BlockingIO]
  ): Props =
    SmallestMailboxPool(config.parallelism)
      .props(props(config, contentRootManager, fs, exec))
}
