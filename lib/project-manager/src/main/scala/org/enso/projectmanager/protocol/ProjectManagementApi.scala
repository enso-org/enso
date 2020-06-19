package org.enso.projectmanager.protocol

import java.util.UUID

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.projectmanager.data.{ProjectMetadata, Socket}

/**
  * The project management JSON RPC API provided by the project manager.
  * See [[https://github.com/luna/enso/blob/main/docs/language-server/README.md]]
  * for message specifications.
  */
object ProjectManagementApi {

  case object ProjectCreate extends Method("project/create") {

    case class Params(name: String)

    case class Result(projectId: UUID)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ProjectCreate.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ProjectCreate.Result
    }
  }

  case object ProjectDelete extends Method("project/delete") {

    case class Params(projectId: UUID)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ProjectDelete.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ProjectOpen extends Method("project/open") {

    case class Params(projectId: UUID)

    case class Result(
      languageServerJsonAddress: Socket,
      languageServerBinaryAddress: Socket
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = ProjectOpen.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ProjectOpen.Result
    }
  }

  case object ProjectClose extends Method("project/close") {

    case class Params(projectId: UUID)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ProjectClose.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ProjectList extends Method("project/list") {

    case class Params(numberOfProjects: Option[Int])

    case class Result(projects: List[ProjectMetadata])

    implicit val hasParams = new HasParams[this.type] {
      type Params = ProjectList.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ProjectList.Result
    }
  }

  case class ProjectNameValidationError(msg: String) extends Error(4001, msg)

  case class ProjectDataStoreError(msg: String) extends Error(4002, msg)

  case object ProjectExistsError
      extends Error(4003, "Project with the provided name exists")

  case object ProjectNotFoundError
      extends Error(4004, "Project with the provided id does not exist")

  case class ProjectOpenError(msg: String) extends Error(4005, msg)

  case object ProjectNotOpenError
      extends Error(4006, "Cannot close project that is not open")

  case object ProjectOpenByOtherPeersError
      extends Error(
        4007,
        "Cannot close project because it is open by other peers"
      )

  case object CannotRemoveOpenProjectError
      extends Error(4008, "Cannot remove open project")

  case class ProjectCloseError(msg: String) extends Error(4009, msg)

}
