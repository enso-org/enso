package org.enso.languageserver.websocket.json

import io.circe.literal._

class SuggestionsHandlerTest extends BaseServerTest {

  "SuggestionsHandler" must {

    "get suggestions database version" in {
      val client = getInitialisedWsClient()

      client.send(json.getSuggestionsDatabaseVersion(0))
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "version" : 0
            }
          }
      """)
    }

    "get suggestions database" in {
      val client = getInitialisedWsClient()

      client.send(json.getSuggestionsDatabase(0))
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "entries" : [
              ],
              "currentVersion" : 0
            }
          }
      """)
    }
  }

  object json {

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

    def ok(reqId: Long) =
      json"""
        { "jsonrpc": "2.0",
          "id": $reqId,
          "result": null
        }
      """
  }

}
