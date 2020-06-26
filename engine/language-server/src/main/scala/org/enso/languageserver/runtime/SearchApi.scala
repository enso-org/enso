package org.enso.languageserver.runtime

import org.enso.jsonrpc.{Error, HasParams, HasResult, Method, Unused}
import org.enso.languageserver.runtime.SearchProtocol.SuggestionsDatabaseUpdate

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

  case object SuggestionsDatabaseError
      extends Error(7001, "Suggestions database error")
}
