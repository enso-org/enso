package org.enso.languageserver.runtime

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.languageserver.runtime.SearchProtocol.{
  SuggestionId,
  SuggestionKind,
  SuggestionsDatabaseUpdate
}
import org.enso.text.editing.model.Position

/**
  * The execution JSON RPC API provided by the language server.
  *
  * @see `docs/language-server/protocol-language-server.md`
  */
object SearchApi {

  case object SuggestionsDatabaseUpdates
      extends Method("search/suggestionsDatabaseUpdates") {

    case class Params(
      updates: Seq[SuggestionsDatabaseUpdate],
      currentVersion: Long
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = SuggestionsDatabaseUpdates.Params
    }
  }

  case object GetSuggestionsDatabase
      extends Method("search/getSuggestionsDatabase") {

    case class Result(
      entries: Seq[SuggestionsDatabaseUpdate],
      currentVersion: Long
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = GetSuggestionsDatabase.Result
    }
  }

  case object GetSuggestionsDatabaseVersion
      extends Method("search/getSuggestionsDatabaseVersion") {

    case class Result(version: Long)

    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = GetSuggestionsDatabaseVersion.Result
    }
  }

  case object Completion extends Method("search/completion") {

    case class Params(
      module: String,
      position: Position,
      selfType: Option[String],
      returnType: Option[String],
      tags: Option[Seq[SuggestionKind]]
    )

    case class Result(results: Seq[SuggestionId], currentVersion: Long)

    implicit val hasParams = new HasParams[this.type] {
      type Params = Completion.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Completion.Result
    }
  }

  case object SuggestionsDatabaseError
      extends Error(7001, "Suggestions database error")
}
