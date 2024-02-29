package org.enso.projectmanager.protocol

import org.enso.jsonrpc.{HasParams, HasResult, Method}
import org.enso.projectmanager.service.filesystem.FileSystemEntry

import java.io.File

object FileSystemManagementApi {

  case object FileSystemList extends Method("filesystem/list") {

    case class Params(path: File)

    case class Result(projects: Seq[FileSystemEntry])

    implicit val hasParams: HasParams.Aux[this.type, FileSystemList.Params] =
      new HasParams[this.type] {
        type Params = FileSystemList.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, FileSystemList.Result] =
      new HasResult[this.type] {
        type Result = FileSystemList.Result
      }
  }
}
