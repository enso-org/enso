package org.enso.languageserver.filemanager

object FileManagerProtocol {

  /**
    * Requests the Language Server write textual content to an arbitrary file.
    *
    * @param path a path to a file
    * @param content a textual content
    */
  case class WriteFile(path: Path, content: String)

  /**
    * Signals file manipulation status.
    *
    * @param result either file system failure or unit representing success
    */
  case class WriteFileResult(result: Either[FileSystemFailure, Unit])

  /**
    * Requests the Language Server read a file.
    *
    * @param path a path to a file
    */
  case class ReadFile(path: Path)

  /**
    * Returns a result of reading a file.
    *
    * @param result either file system failure or content of a file
    */
  case class ReadFileResult(result: Either[FileSystemFailure, String])

  /**
    * Requests the Language Server create a file system object.
    *
    * @param `object` a file system object
    */
  case class CreateFile(`object`: FileSystemObject)

  /**
    * Returns a result of creating a file system object.
    *
    * @param result either file system failure or unit representing success
    */
  case class CreateFileResult(result: Either[FileSystemFailure, Unit])

}
