package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.Suggestion

class SuggestionsDatabaseEventsListenerTest extends BaseServerTest {

  "SuggestionsDatabaseEventListener" must {

    "acquire and release capabilities" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      client.send(json.releaseSuggestionsDatabaseUpdatesCapability(1))
      client.expectJson(json.ok(1))
    }

    "send suggestions database add atom notifications" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(0, suggestion.atom))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 0,
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
            "currentVersion" : 0
          }
        }
        """)
    }

    "send suggestions database add method notifications" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(0, suggestion.method))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 0,
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
                  "documentation" : "My doc"
                }
              }
            ],
            "currentVersion" : 0
          }
        }
        """)
    }

    "send suggestions database add function notifications" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(0, suggestion.function))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 0,
                "suggestion" : {
                  "type" : "function",
                  "name" : "print",
                  "arguments" : [
                  ],
                  "returnType" : "IO",
                  "scope" : {
                    "start" : 7,
                    "end" : 10
                  }
                }
              }
            ],
            "currentVersion" : 0
          }
        }
      """)
    }

    "send suggestions database add local notifications" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Add(0, suggestion.local))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Add",
                "id" : 0,
                "suggestion" : {
                  "type" : "local",
                  "name" : "x",
                  "returnType" : "Number",
                  "scope" : {
                    "start" : 15,
                    "end" : 17
                  }
                }
              }
            ],
            "currentVersion" : 0
          }
        }
        """)
    }

    "send suggestions database modify notifications" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(
            Api.SuggestionsDatabaseUpdate.Modify(
              id   = 0,
              name = Some("foo"),
              arguments = Some(
                Seq(
                  Suggestion.Argument("a", "Any", true, false, None),
                  Suggestion.Argument("b", "Any", false, true, Some("77"))
                )
              ),
              selfType      = Some("MyType"),
              returnType    = Some("IO"),
              documentation = None,
              scope         = Some(Suggestion.Scope(12, 24))
            )
          )
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Update",
                "id" : 0,
                "name" : "foo",
                "arguments" : [
                  {
                    "name" : "a",
                    "reprType" : "Any",
                    "isSuspended" : true,
                    "hasDefault" : false,
                    "defaultValue" : null
                  },
                  {
                    "name" : "b",
                    "reprType" : "Any",
                    "isSuspended" : false,
                    "hasDefault" : true,
                    "defaultValue" : "77"
                  }
                ],
                "selfType" : "MyType",
                "returnType" : "IO",
                "scope" : {
                  "start" : 12,
                  "end" : 24
                }
              }
            ],
            "currentVersion" : 0
          }
        }
        """)
    }

    "send suggestions database remove notifications" in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      system.eventStream.publish(
        Api.SuggestionsDatabaseUpdateNotification(
          Seq(Api.SuggestionsDatabaseUpdate.Remove(101))
        )
      )
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Delete",
                "id" : 101
              }
            ],
            "currentVersion" : 0
          }
        }
        """)
    }
  }

  object suggestion {

    val atom: Suggestion.Atom =
      Suggestion.Atom(
        name          = "MyType",
        arguments     = Seq(Suggestion.Argument("a", "Any", false, false, None)),
        returnType    = "MyAtom",
        documentation = None
      )

    val method: Suggestion.Method =
      Suggestion.Method(
        name = "foo",
        arguments = Seq(
          Suggestion.Argument("this", "MyType", false, false, None),
          Suggestion.Argument("foo", "Number", false, true, Some("42"))
        ),
        selfType      = "MyType",
        returnType    = "Number",
        documentation = Some("My doc")
      )

    val function: Suggestion.Function =
      Suggestion.Function(
        name       = "print",
        arguments  = Seq(),
        returnType = "IO",
        scope      = Suggestion.Scope(7, 10)
      )

    val local: Suggestion.Local =
      Suggestion.Local(
        name       = "x",
        returnType = "Number",
        scope      = Suggestion.Scope(15, 17)
      )
  }

  object json {

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

    def ok(reqId: Long) =
      json"""
        { "jsonrpc": "2.0",
          "id": $reqId,
          "result": null
        }
      """
  }

}
