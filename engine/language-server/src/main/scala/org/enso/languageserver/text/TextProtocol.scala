package org.enso.languageserver.text

import org.enso.languageserver.data.{CapabilityRegistration, ClientId}
import org.enso.languageserver.filemanager.{FileSystemFailure, Path}
import org.enso.languageserver.session.JsonSession

object TextProtocol {

  /** Requests the language server to open a file on behalf of a given user.
    *
    * @param rpcSession the client opening the file.
    * @param path the file path.
    */
  case class OpenFile(rpcSession: JsonSession, path: Path)

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
  case class CloseFile(clientId: ClientId, path: Path)

  /** Signals file close status.
    */
  sealed trait CloseFileResult

  /** Confirms that a file was successfully closed.
    */
  case object FileClosed extends CloseFileResult

  /** Signals that a file wasn't opened.
    */
  case object FileNotOpened

  /** Requests the language server to apply a series of edits to the buffer.
    *
    * @param clientId the client closing the file.
    * @param edit a diff describing changes made to a file
    */
  case class ApplyEdit(clientId: ClientId, edit: FileEdit)

  /** Signals the result of applying a series of edits.
    */
  sealed trait ApplyEditResult

  /** Signals that all edits were applied successfully.
    */
  case object ApplyEditSuccess extends ApplyEditResult

  /** A base trait for all failures regarding editing.
    */
  sealed trait ApplyEditFailure extends ApplyEditResult

  /** Signals that the client doesn't hold write lock to the buffer.
    */
  case object WriteDenied extends ApplyEditFailure

  /** Signals that validation has failed for a series of edits.
    *
    * @param msg a validation message
    */
  case class TextEditValidationFailed(msg: String) extends ApplyEditFailure

  /** Signals that version provided by a client doesn't match to the version
    * computed by the server.
    *
    * @param clientVersion a version send by the client
    * @param serverVersion a version computed by the server
    */
  case class TextEditInvalidVersion(
    clientVersion: Buffer.Version,
    serverVersion: Buffer.Version
  ) extends ApplyEditFailure

  /** A notification sent by the Language Server, notifying a client about
    * edits made by the write lock holder.
    *
    * @param changes a series of edits
    */
  case class TextDidChange(changes: List[FileEdit])

  /** Requests the language server to save a file on behalf of a given user.
    *
    * @param clientId the client closing the file.
    * @param path the file path.
    * @param currentVersion the current version evaluated on the client side.
    */
  case class SaveFile(
    clientId: ClientId,
    path: Path,
    currentVersion: Buffer.Version
  )

  /** Signals the result of saving a file.
    */
  sealed trait SaveFileResult

  /** Signals that saving a file was executed successfully.
    */
  case object FileSaved extends SaveFileResult

  /** Signals that the client doesn't hold write lock to the buffer.
    */
  case object SaveDenied extends SaveFileResult

  /** Signals that version provided by a client doesn't match to the version
    * computed by the server.
    *
    * @param clientVersion a version send by the client
    * @param serverVersion a version computed by the server
    */
  case class SaveFileInvalidVersion(
    clientVersion: Buffer.Version,
    serverVersion: Buffer.Version
  ) extends SaveFileResult

  /** Signals that saving a file failed due to IO error.
    *
    * @param fsFailure a filesystem failure
    */
  case class SaveFailed(fsFailure: FileSystemFailure) extends SaveFileResult

}
