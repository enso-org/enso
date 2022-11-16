package org.enso.languageserver.vcsmanager

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.languageserver.filemanager.Path

/** The VCS JSON RPC API provided by the language server.
  * See [[https://github.com/enso-org/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object VcsManagerApi {
  case object InitVcs extends Method("vcs/init") {
    case class Params(root: Path)

    implicit val hasParams = new HasParams[this.type] {
      type Params = InitVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object SaveVcs extends Method("vcs/save") {
    case class Params(root: Path, name: Option[String])
    case class Result(commitId: String, message: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = SaveVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = SaveVcs.Result
    }
  }

  case object StatusVcs extends Method("vcs/status") {
    case class Params(root: Path)
    case class Result(dirty: Boolean, changed: List[Path], lastSave: Save)
    case class Save(commitId: String, message: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = StatusVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = StatusVcs.Result
    }
  }

  case object RestoreVcs extends Method("vcs/restore") {
    case class Params(root: Path, commitId: Option[String])

    implicit val hasParams = new HasParams[this.type] {
      type Params = RestoreVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ListVcs extends Method("vcs/list") {
    case class Params(root: Path, limit: Option[Int])
    case class Result(saves: List[Save])
    case class Save(commitId: String, message: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ListVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ListVcs.Result
    }
  }

  // Errors

  case class VCSError(override val message: String) extends Error(1000, message)

  case object ContentRootNotFoundError
      extends Error(1001, "Content root not found")

  case object ProjectNotFound extends Error(1002, "Project not found")

  case object VCSNotFound
      extends Error(1003, "Project is not under version control")

  case object SaveNotFound extends Error(1004, "Requested save not found")

  case object VCSAlreadyExists
      extends Error(1005, "Requested project is already under version control")

}
