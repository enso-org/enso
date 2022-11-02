package org.enso.languageserver.vcsmanager

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.languageserver.filemanager.Path

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

  case object CommitVcs extends Method("vcs/commit") {
    case class Params(root: Path, name: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = CommitVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ModifiedVcs extends Method("vcs/modified") {
    case class Params(root: Path)
    case class Result(dirty: Boolean)

    implicit val hasParams = new HasParams[this.type] {
      type Params = ModifiedVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ModifiedVcs.Result
    }
  }

  case object RestoreVcs extends Method("vcs/restore") {
    case class Params(root: Path)

    implicit val hasParams = new HasParams[this.type] {
      type Params = RestoreVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ListVcs extends Method("vcs/listTags") {
    case class Params(root: Path)
    case class Result(tags: List[String])

    implicit val hasParams = new HasParams[this.type] {
      type Params = ListVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = ListVcs.Result
    }
  }

  // Errors
  case object ContentRootNotFoundError
      extends Error(1001, "Content root not found")

  case object ProjectNotFound extends Error(1003, "Project not found")

  case class VcsError(override val message: String) extends Error(1000, message)

}
