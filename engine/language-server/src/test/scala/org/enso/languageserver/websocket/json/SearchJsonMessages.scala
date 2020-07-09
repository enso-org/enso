package org.enso.languageserver.websocket.json

import io.circe.literal._

object SearchJsonMessages {

  def acquireSuggestionsDatabaseUpdatesCapability(reqId: Long) =
    json"""
        { "jsonrpc": "2.0",
          "method": "capability/acquire",
          "id": $reqId,
          "params": {
            "method": "search/receivesSuggestionsDatabaseUpdates",
            "registerOptions": {}
          }
        }
      """

  def releaseSuggestionsDatabaseUpdatesCapability(reqId: Long) =
    json"""
        { "jsonrpc": "2.0",
          "method": "capability/release",
          "id": $reqId,
          "params": {
            "method": "search/receivesSuggestionsDatabaseUpdates",
            "registerOptions": {}
          }
        }
      """

  def getSuggestionsDatabaseVersion(reqId: Long) =
    json"""
        { "jsonrpc": "2.0",
          "method": "search/getSuggestionsDatabaseVersion",
          "id": $reqId,
          "params": null
        }
      """

  def getSuggestionsDatabase(reqId: Long) =
    json"""
        { "jsonrpc": "2.0",
          "method": "search/getSuggestionsDatabase",
          "id": $reqId,
          "params": null
        }
      """

  def completion(reqId: Long) =
    json"""
        { "jsonrpc": "2.0",
          "method": "search/completion",
          "id": $reqId,
          "params": {
            "module": "Test.Main",
            "position": {
              "line": 0,
              "character": 0
            }
          }
        }
      """

  def ok(reqId: Long) =
    json"""
        { "jsonrpc": "2.0",
          "id": $reqId,
          "result": null
        }
      """
}
