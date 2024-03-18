package org.enso.projectmanager.protocol

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}
import org.enso.projectmanager.service.filesystem.FileSystemEntry

import java.io.File

object FileSystemManagementApi {

  case object FileSystemList extends Method("filesystem/list") {

    case class Params(path: File)

    case class Result(entries: Seq[FileSystemEntry])

    implicit val hasParams: HasParams.Aux[this.type, FileSystemList.Params] =
      new HasParams[this.type] {
        type Params = FileSystemList.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, FileSystemList.Result] =
      new HasResult[this.type] {
        type Result = FileSystemList.Result
      }
  }

  case object FileSystemCreateDirectory
      extends Method("filesystem/createDirectory") {

    case class Params(path: File)

    type Result = Unused.type
    val Result = Unused

    implicit val hasParams
      : HasParams.Aux[this.type, FileSystemCreateDirectory.Params] =
      new HasParams[this.type] {
        type Params = FileSystemCreateDirectory.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object FileSystemDeleteDirectory
      extends Method("filesystem/deleteDirectory") {

    case class Params(path: File)

    type Result = Unused.type
    val Result = Unused

    implicit val hasParams
      : HasParams.Aux[this.type, FileSystemDeleteDirectory.Params] =
      new HasParams[this.type] {
        type Params = FileSystemDeleteDirectory.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object FileSystemMoveDirectory
      extends Method("filesystem/moveDirectory") {

    case class Params(from: File, to: File)

    type Result = Unused.type
    val Result = Unused

    implicit val hasParams
      : HasParams.Aux[this.type, FileSystemMoveDirectory.Params] =
      new HasParams[this.type] {
        type Params = FileSystemMoveDirectory.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }
}
