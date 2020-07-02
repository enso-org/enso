package org.enso.languageserver.websocket.json
import io.circe.literal._
import org.enso.languageserver.websocket.json.{SearchJsonMessages => json}

class SuggestionsHandlerTest extends BaseServerTest {

  "SuggestionsHandler" must {

    "get initial suggestions database version" in {
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

    "get initial suggestions database" in {
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

    "reply to completion request" in {
      val client = getInitialisedWsClient()

      client.send(json.completion(0))
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "results" : [
              ],
              "currentVersion" : 0
            }
          }
      """)
    }
  }

}
