package org.enso.projectmanager.infrastructure.file

import java.io.{File, FileNotFoundException, InputStream, OutputStream}
import java.nio.file.{
  AccessDeniedException,
  NoSuchFileException,
  NotDirectoryException
}
import org.apache.commons.io.{FileExistsException, FileUtils, IOUtils}
import org.enso.projectmanager.control.effect.syntax._
import org.enso.projectmanager.control.effect.{ErrorChannel, Sync}
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem.Encoding
import org.enso.projectmanager.infrastructure.file.FileSystemFailure._

import scala.concurrent.duration.FiniteDuration

/** ZIO implementation of [[FileSystem]]. This implementation uses blocking
  * API to access data on the disk.
  *
  * @param ioTimeout a timeout for IO operations
  */
class BlockingFileSystem[F[+_, +_]: Sync: ErrorChannel](
  ioTimeout: FiniteDuration
) extends FileSystem[F] {

  /** @inheritdoc */
  override def readTextFile(file: File): F[FileSystemFailure, String] =
    Sync[F]
      .blockingOp { FileUtils.readFileToString(file, Encoding) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /** @inheritdoc */
  override def readFile(
    file: File,
    output: OutputStream
  ): F[FileSystemFailure, Int] = {
    Sync[F]
      .blockingOp {
        IOUtils.copy(java.nio.file.Files.newInputStream(file.toPath), output)
      }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)
  }

  /** Writes binary content to a file.
    *
    * @param file path to the file
    * @param contents a textual contents of the file
    * @return either [[FileSystemFailure]] or Unit
    */
  def writeFile(file: File, contents: InputStream): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp { FileUtils.copyInputStreamToFile(contents, file) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /** Writes textual content to a file.
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

  /** @inheritdoc */
  override def createDir(path: File): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp { FileUtils.forceMkdir(path) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /** @inheritdoc */
  override def remove(path: File): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp { FileUtils.forceDelete(path) }
      .mapError(toFsFailure)
      .timeoutFail(OperationTimeout)(ioTimeout)

  /** @inheritdoc */
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

  /** @inheritdoc */
  override def copy(from: File, to: File): F[FileSystemFailure, Unit] =
    Sync[F]
      .blockingOp {
        if (to.isDirectory) {
          FileUtils.copyToDirectory(from, to)
        } else if (from.isDirectory) {
          FileUtils.copyDirectory(from, to)
        } else {
          FileUtils.copyFile(from, to)
        }
      }
      .mapError(toFsFailure)

  /** @inheritdoc */
  override def exists(file: File): F[FileSystemFailure, Boolean] =
    Sync[F]
      .blockingOp(file.exists())
      .mapError(toFsFailure)

  /** @inheritdoc */
  override def list(directory: File): F[FileSystemFailure, List[File]] =
    Sync[F]
      .blockingOp {
        val res = directory.listFiles()
        if (res eq null) {
          throw new NotDirectoryException(s"Not a directory: $directory")
        } else {
          res.toList
        }
      }
      .mapError(toFsFailure)

  private val toFsFailure: Throwable => FileSystemFailure = {
    case _: FileNotFoundException  => FileNotFound
    case _: NotDirectoryException  => NotDirectory
    case _: NoSuchFileException    => FileNotFound
    case _: FileExistsException    => FileExists
    case ex: AccessDeniedException => AccessDenied(ex.getFile)
    case ex                        => GenericFileSystemFailure(ex.getMessage)
  }

}

object BlockingFileSystem {

  val Encoding = "UTF-8"

}
