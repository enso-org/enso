package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.runtime.Suggestions
import org.enso.languageserver.websocket.json.{SearchJsonMessages => json}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.FlakySpec

class SuggestionsHandlerEventsTest extends BaseServerTest with FlakySpec {

  "SuggestionsHandlerEvents" must {

    "send suggestions database notifications" taggedAs Flaky in {
      val client = getInitialisedWsClient()
      system.eventStream.publish(ProjectNameChangedEvent("Test"))

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
                  "module" : "Test.Main",
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
                  "externalId" : "ea9d7734-26a7-4f65-9dd9-c648eaf57d63",
                  "module" : "Test.Main",
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
                  "externalId" : "78d452ce-ed48-48f1-b4f2-b7f45f8dff89",
                  "module" : "Test.Main",
                  "name" : "print",
                  "arguments" : [
                  ],
                  "returnType" : "IO",
                  "scope" : {
                    "start" : {
                      "line" : 1,
                      "character" : 9
                    },
                    "end" : {
                      "line" : 1,
                      "character" : 22
                    }
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
                  "externalId" : "dc077227-d9b6-4620-9b51-792c2a69419d",
                  "module" : "Test.Main",
                  "name" : "x",
                  "returnType" : "Number",
                  "scope" : {
                    "start" : {
                      "line" : 21,
                      "character" : 0
                    },
                    "end" : {
                      "line" : 89,
                      "character" : 0
                    }
                  }
                }
              }
            ],
            "currentVersion" : 4
          }
        }
        """)

      // get suggestions database
      client.send(json.getSuggestionsDatabase(0))
      client.expectJson(json"""
        { "jsonrpc" : "2.0",
          "id" : 0,
          "result" : {
            "entries" : [
              {
                "id" : 3,
                "suggestion" : {
                  "type" : "function",
                  "externalId" : ${Suggestions.function.externalId.get},
                  "module" : "Test.Main",
                  "name" : "print",
                  "arguments" : [
                  ],
                  "returnType" : "IO",
                  "scope" : {
                    "start" : {
                      "line" : 1,
                      "character" : 9
                    },
                    "end" : {
                      "line" : 1,
                      "character" : 22
                    }
                  }
                }
              },
              {
                "id" : 1,
                "suggestion" : {
                  "type" : "atom",
                  "module" : "Test.Main",
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
              },
              {
                "id" : 2,
                "suggestion" : {
                  "type" : "method",
                  "externalId" : ${Suggestions.method.externalId.get},
                  "module" : "Test.Main",
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
              },
              {
                "id" : 4,
                "suggestion" : {
                  "type" : "local",
                  "externalId" : ${Suggestions.local.externalId.get},
                  "module" : "Test.Main",
                  "name" : "x",
                  "returnType" : "Number",
                  "scope" : {
                    "start" : {
                      "line" : 21,
                      "character" : 0
                    },
                    "end" : {
                      "line" : 89,
                      "character" : 0
                    }
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
