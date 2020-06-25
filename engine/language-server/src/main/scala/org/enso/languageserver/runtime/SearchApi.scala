package org.enso.languageserver.runtime

import org.enso.jsonrpc.{HasParams, Method}
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

}
