package org.enso.languageserver.filemanager

object FileManagerProtocol {

  /**
    * Requests the Language Server write textual content to an arbitrary file.
    *
    * @param path a path to a file
    * @param content a textual content
    */
  case class FileWrite(path: Path, content: String)

  /**
    * Signals file manipulation status.
    *
    * @param result either file system failure or unit representing success
    */
  case class FileWriteResult(result: Either[FileSystemFailure, Unit])

  /**
    * Requests the Language Server read a file.
    *
    * @param path a path to a file
    */
  case class FileRead(path: Path)

  /**
    * Returns a result of reading a file.
    *
    * @param result either file system failure or content of a file
    */
  case class FileReadResult(result: Either[FileSystemFailure, String])

}
