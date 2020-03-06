package org.enso.languageserver.text

import org.enso.languageserver.data.{CapabilityRegistration, Client}
import org.enso.languageserver.filemanager.{FileSystemFailure, Path}

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

}
