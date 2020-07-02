package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.jsonrpc.test.FlakySpec
import org.enso.languageserver.runtime.Suggestions
import org.enso.languageserver.websocket.json.{SearchJsonMessages => json}
import org.enso.polyglot.runtime.Runtime.Api
import org.scalatest.BeforeAndAfter

class SuggestionsDatabaseEventsListenerTest
    extends BaseServerTest
    with BeforeAndAfter
    with FlakySpec {

  lazy val client = getInitialisedWsClient()

  "SuggestionsDatabaseEventListener" must {

    "send suggestions database notifications" taggedAs Flaky in {

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      // add atom
      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(Suggestions.atom))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 1,
                "suggestion" : {
                  "type" : "atom",
                  "name" : "MyType",
                  "arguments" : [
                    {
                      "name" : "a",
                      "reprType" : "Any",
                      "isSuspended" : false,
                      "hasDefault" : false,
                      "defaultValue" : null
                    }
                  ],
                  "returnType" : "MyAtom"
                }
              }
            ],
            "currentVersion" : 1
          }
        }
        """)

      // add method
      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(Suggestions.method))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 2,
                "suggestion" : {
                  "type" : "method",
                  "name" : "foo",
                  "arguments" : [
                    {
                      "name" : "this",
                      "reprType" : "MyType",
                      "isSuspended" : false,
                      "hasDefault" : false,
                      "defaultValue" : null
                    },
                    {
                      "name" : "foo",
                      "reprType" : "Number",
                      "isSuspended" : false,
                      "hasDefault" : true,
                      "defaultValue" : "42"
                    }
                  ],
                  "selfType" : "MyType",
                  "returnType" : "Number",
                  "documentation" : "Lovely"
                }
              }
            ],
            "currentVersion" : 2
          }
        }
        """)

      // add function
      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(Suggestions.function))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 3,
                "suggestion" : {
                  "type" : "function",
                  "name" : "print",
                  "arguments" : [
                  ],
                  "returnType" : "IO",
                  "scope" : {
                    "start" : 9,
                    "end" : 22
                  }
                }
              }
            ],
            "currentVersion" : 3
          }
        }
      """)

      // add local
      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(Suggestions.local))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 4,
                "suggestion" : {
                  "type" : "local",
                  "name" : "x",
                  "returnType" : "Number",
                  "scope" : {
                    "start" : 34,
                    "end" : 68
                  }
                }
              }
            ],
            "currentVersion" : 4
          }
        }
        """)

      // remove items
      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(
            Api.SuggestionsDatabaseUpdate.Remove(Suggestions.method),
            Api.SuggestionsDatabaseUpdate.Remove(Suggestions.function)
          )
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Remove",
                "id" : 2
              },
              {
                "type" : "Remove",
                "id" : 3
              }
            ],
            "currentVersion" : 6
          }
        }
        """)

    }
  }

}
