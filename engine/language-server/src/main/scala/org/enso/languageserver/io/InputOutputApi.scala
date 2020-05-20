package org.enso.languageserver.io
import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

/**
  * The input/output JSON RPC API provided by the language server.
  *
  * @see [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
  */
object InputOutputApi {

  case object RedirectStandardOutput
      extends Method("io/redirectStandardOutput") {

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object SuppressStandardOutput
      extends Method("io/suppressStandardOutput") {

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object StandardOutputAppended
      extends Method("io/standardOutputAppended") {

    case class Params(output: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = StandardOutputAppended.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object RedirectStandardError extends Method("io/redirectStandardError") {

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object SuppressStandardError extends Method("io/suppressStandardError") {

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object StandardErrorAppended extends Method("io/standardErrorAppended") {

    case class Params(output: String)

    implicit val hasParams = new HasParams[this.type] {
      type Params = StandardErrorAppended.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object FeedStandardInput extends Method("io/feedStandardInput") {

    case class Params(input: String, isLineTerminated: Boolean)

    implicit val hasParams = new HasParams[this.type] {
      type Params = FeedStandardInput.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object WaitingForStandardInput
      extends Method("io/waitingForStandardInput") {

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

}
