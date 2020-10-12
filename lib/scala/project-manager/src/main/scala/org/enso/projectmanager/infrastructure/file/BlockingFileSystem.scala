package org.enso.projectmanager.infrastructure.file
import java.io.{File, FileNotFoundException}
import java.nio.file.{AccessDeniedException, NoSuchFileException}

import org.apache.commons.io.{FileExistsException, FileUtils}
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem.Encoding
import org.enso.projectmanager.infrastructure.file.FileSystemFailure._

import scala.concurrent.duration.FiniteDuration

/**
  * ZIO implementation of [[FileSystem]]. This implementation uses blocking
  * API to access data on the disk.
  *
  * @param ioTimeout a timeout for IO operations
  */
class BlockingFileSystem[F[+_, +_]: Sync: ErrorChannel](
  ioTimeout: FiniteDuration
) extends FileSystem[F] {

  /**
    * Reads the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  override def readFile(file: File): F[FileSystemFailure, String] =
    Sync[F]
      .blockingOp { FileUtils.readFileToString(file, Encoding) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /**
    * Writes textual content to a file.
    *
    * @param file path to the file
    * @param contents a textual contents of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  override def overwriteFile(
    file: File,
    contents: String
  ): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp { FileUtils.write(file, contents, Encoding) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /**
    * Deletes the specified directory recursively.
    *
    * @param path a path to the directory
    * @return either [[FileSystemFailure]] or Unit
    */
  override def removeDir(path: File): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp { FileUtils.deleteDirectory(path) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /** @inheritdoc * */
  override def move(from: File, to: File): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp {
        if (to.isDirectory) {
          val createDestDir = false
          FileUtils.moveToDirectory(from, to, createDestDir)
        } else if (from.isDirectory) {
          FileUtils.moveDirectory(from, to)
        } else {
          FileUtils.moveFile(from, to)
        }
      }
      .mapError(toFsFailure)

  /** @inheritdoc * */
  override def exists(file: File): F[FileSystemFailure, Boolean] =
    Sync[F]
      .blockingOp(file.exists())
      .mapError(toFsFailure)

  private val toFsFailure: Throwable => FileSystemFailure = {
    case _: FileNotFoundException => FileNotFound
    case _: NoSuchFileException   => FileNotFound
    case _: FileExistsException   => FileExists
    case _: AccessDeniedException => AccessDenied
    case ex                       => GenericFileSystemFailure(ex.getMessage)
  }

}

object BlockingFileSystem {

  val Encoding = "UTF-8"

}
