package org.enso.languageserver.filemanager

import java.io.{File, FileNotFoundException, IOException}
import java.nio.file._

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import org.apache.commons.io.FileUtils

/**
  * File manipulation facility.
  *
  * @tparam F represents target monad
  */
class FileSystem[F[_]: Sync] extends FileSystemApi[F] {

  /**
    * Writes textual content to a file.
    *
    * @param file path to the file
    * @param content    a textual content of the file
    * @return either FileSystemFailure or Unit
    */
  override def write(
    file: File,
    content: String
  ): F[Either[FileSystemFailure, Unit]] =
    Sync[F].delay {
      Either
        .catchOnly[IOException] {
          FileUtils.write(file, content, "UTF-8")
        }
        .leftMap(errorHandling)
    }

  /**
    * Reads the contents of a textual file.
    *
    * @param file path to the file
    * @return either [[FileSystemFailure]] or the content of a file as a String
    */
  override def read(file: File): F[Either[FileSystemFailure, String]] =
    Sync[F].delay {
      Either
        .catchOnly[IOException] {
          FileUtils.readFileToString(file, "UTF-8")
        }
        .leftMap(errorHandling)
    }

  /**
    * Deletes the specified file or directory recursively.
    *
    * @param file path to the file or directory
    * @return either [[FileSystemFailure]] or Unit
    */
  def delete(file: File): F[Either[FileSystemFailure, Unit]] =
    Sync[F].delay {
      Either
        .catchOnly[IOException] {
          if (file.isDirectory) {
            FileUtils.deleteDirectory(file)
          } else {
            Files.delete(file.toPath)
          }
        }
        .leftMap(errorHandling)
    }

  /**
    * Creates an empty file with parent directory.
    *
    * @param file path to the file
    * @return
    */
  override def createFile(file: File): F[Either[FileSystemFailure, Unit]] = {
    val op =
      for {
        _ <- EitherT { createDirectory(file.getParentFile) }
        _ <- EitherT { createEmptyFile(file)               }
      } yield ()

    op.value
  }

  private def createEmptyFile(file: File): F[Either[FileSystemFailure, Unit]] =
    Sync[F].delay {
      Either
        .catchOnly[IOException] {
          file.createNewFile()
        }
        .leftMap(errorHandling)
        .map(_ => ())
    }

  /**
    * Creates a directory, including any necessary but nonexistent parent
    * directories.
    *
    * @param file path to the file
    * @return
    */
  override def createDirectory(
    file: File
  ): F[Either[FileSystemFailure, Unit]] =
    Sync[F].delay {
      Either
        .catchOnly[IOException] {
          FileUtils.forceMkdir(file)
        }
        .leftMap(errorHandling)
    }

  private val errorHandling: IOException => FileSystemFailure = {
    case _: FileNotFoundException => FileNotFound
    case _: NoSuchFileException   => FileNotFound
    case _: AccessDeniedException => AccessDenied
    case ex                       => GenericFileSystemFailure(ex.getMessage)
  }

}
