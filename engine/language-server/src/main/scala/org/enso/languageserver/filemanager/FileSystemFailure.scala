package org.enso.languageserver.filemanager

/**
  * Represents file system failures.
  */
sealed trait FileSystemFailure

/**
  * Informs that the requested content root cannot be found.
  */
case object ContentRootNotFound extends FileSystemFailure

/**
  * Signals that a user doesn't have access to a file.
  */
case object AccessDenied extends FileSystemFailure

/**
  * Signals that the file cannot be found.
  */
case object FileNotFound extends FileSystemFailure

/**
  * Signals that the file already exists.
  */
case object FileExists extends FileSystemFailure

/**
  * Signal that the operation timed out.
  */
case object OperationTimeout extends FileSystemFailure

/**
  * Signals file system specific errors.
  *
  * @param reason a reason of failure
  */
case class GenericFileSystemFailure(reason: String) extends FileSystemFailure
