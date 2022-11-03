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
    case class Params(root: Path, name: Option[String])

    implicit val hasParams = new HasParams[this.type] {
      type Params = CommitVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object StatusVcs extends Method("vcs/status") {
    case class Params(root: Path)
    case class Result(dirty: Boolean, changed: List[Path], lastCommit: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = StatusVcs.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = StatusVcs.Result
    }
  }

  case object RestoreVcs extends Method("vcs/restore") {
    case class Params(root: Path, name: Option[String])

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

  case class VcsError(override val message: String) extends Error(1000, message)

  case object ContentRootNotFoundError
      extends Error(1001, "Content root not found")

  case object ProjectNotFound extends Error(1002, "Project not found")

  case object RepoNotFound
      extends Error(1003, "Project is not under version control")

  case object NamedSaveNotFound
    extends Error(1004, "Requested save not found")
  case object NamedSaveAlreadyExists
    extends Error(1005, "Requested save already exists")

}
