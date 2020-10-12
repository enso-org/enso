package org.enso.languageserver.websocket.json
import java.util.UUID

import io.circe.literal._
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.websocket.json.{SearchJsonMessages => json}
import org.enso.testkit.FlakySpec

class SuggestionsHandlerTest extends BaseServerTest with FlakySpec {

  "SuggestionsHandler" must {

    "stash messages when initializing" in {
      val client = getInitialisedWsClient()

      client.send(json.getSuggestionsDatabaseVersion(0))
      client.expectNoMessage()
    }

    "get initial suggestions database version" in {
      val client = getInitialisedWsClient()
      system.eventStream.publish(ProjectNameChangedEvent("Test", "Test"))

      client.send(json.getSuggestionsDatabaseVersion(0))
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "result" : {
              "currentVersion" : 0
            }
          }
      """)
    }

    "get initial suggestions database" taggedAs Flaky in {
      val client = getInitialisedWsClient()
      system.eventStream.publish(ProjectNameChangedEvent("Test", "Test"))

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

    "reply to completion request" taggedAs Flaky in {
      val client = getInitialisedWsClient()
      system.eventStream.publish(ProjectNameChangedEvent("Test", "Test"))

      client.send(json"""
        { "jsonrpc": "2.0",
          "method": "search/completion",
          "id": 0,
          "params": {
            "file": {
              "rootId": $testContentRootId,
              "segments": [ "src", "Main.enso" ]
            },
            "position": {
              "line": 0,
              "character": 0
            }
          }
        }
      """)
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

      client.send(json"""
        { "jsonrpc": "2.0",
          "method": "search/completion",
          "id": 0,
          "params": {
            "file": {
              "rootId": $testContentRootId,
              "segments": [ "src", "Foo", "Main.enso" ]
            },
            "position": {
              "line": 0,
              "character": 0
            }
          }
        }
      """)
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

    "reply with error when project root not found" taggedAs Flaky in {
      val client = getInitialisedWsClient()
      system.eventStream.publish(ProjectNameChangedEvent("Test", "Test"))

      client.send(json"""
        { "jsonrpc": "2.0",
          "method": "search/completion",
          "id": 0,
          "params": {
            "file": {
              "rootId": ${UUID.randomUUID()},
              "segments": [ "src", "Main.enso" ]
            },
            "position": {
              "line": 0,
              "character": 0
            }
          }
        }
      """)
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "id" : 0,
            "error" : {
              "code" : 1001,
              "message" : "Content root not found"
            }
          }
      """)
    }
  }

}
