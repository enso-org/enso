package org.enso.languageserver.text

import org.enso.languageserver.data.{CapabilityRegistration, Client}
import org.enso.languageserver.filemanager.{FileSystemFailure, Path}
import org.enso.languageserver.text.editing.model.FileEdit

object TextProtocol {

  /** Requests the language server to open a file on behalf of a given user.
    *
    * @param client the client opening the file.
    * @param path the file path.
    */
  case class OpenFile(client: Client, path: Path)

  /** Sent by the server in response to [[OpenFile]]
    *
    * @param result either a file system failure, or successful opening data.
    */
  case class OpenFileResponse(result: Either[FileSystemFailure, OpenFileResult])

  /** The data carried by a successful file open operation.
    *
    * @param buffer file contents and current version.
    * @param writeCapability a write capability that could have been
    *                        automatically granted.
    */
  case class OpenFileResult(
    buffer: Buffer,
    writeCapability: Option[CapabilityRegistration]
  )

  /** Requests the language server to close a file on behalf of a given user.
    *
    * @param clientId the client closing the file.
    * @param path the file path.
    */
  case class CloseFile(clientId: Client.Id, path: Path)

  /**
    * Signals file close status.
    */
  sealed trait CloseFileResult

  /**
    * Confirms that a file was successfully closed.
    */
  case object FileClosed extends CloseFileResult

  /**
    * Signals that a file wasn't opened.
    */
  case object FileNotOpened

  /**
    * Requests the language server to apply a series of edits to the buffer.
    *
    * @param clientId the client closing the file.
    * @param edit a diff describing changes made to a file
    */
  case class ApplyEdit(clientId: Client.Id, edit: FileEdit)

  /**
    * Signals the result of applying a series of edits.
    */
  sealed trait ApplyEditResult

  /**
    * Signals that all edits were applied successfully.
    */
  case object ApplyEditSuccess extends ApplyEditResult

  /**
    * A base trait for all failures regarding editing.
    */
  sealed trait ApplyEditFailure extends ApplyEditResult

  /**
    * Signals that the client doesn't hold write lock to the buffer.
    */
  case object WriteDenied extends ApplyEditFailure

  /**
    * Signals that validation has failed for a series of edits.
    *
    * @param msg a validation message
    */
  case class TextEditValidationFailed(msg: String) extends ApplyEditFailure

  /**
    * Signals that version provided by a client doesn't match to the version
    * computed by the server.
    *
    * @param clientVersion a version send by the client
    * @param serverVersion a version computed by the server
    */
  case class InvalidVersion(
    clientVersion: Buffer.Version,
    serverVersion: Buffer.Version
  ) extends ApplyEditFailure

  /**
    * A notification sent by the Language Server, notifying a client about
    * edits made by the write lock holder.
    *
    * @param changes a series of edits
    */
  case class TextDidChange(changes: List[FileEdit])

}
