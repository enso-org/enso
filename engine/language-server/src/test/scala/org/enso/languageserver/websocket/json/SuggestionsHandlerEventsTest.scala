package org.enso.languageserver.websocket.json

import java.io.File

import io.circe.literal._
import org.enso.languageserver.search.Suggestions
import org.enso.languageserver.websocket.json.{SearchJsonMessages => json}
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.RetrySpec

class SuggestionsHandlerEventsTest extends BaseServerTest with RetrySpec {

  "SuggestionsHandlerEvents" must {

    "send suggestions database notifications" taggedAs Retry in {
      val client = getInitialisedWsClient()

      client.send(json.acquireSuggestionsDatabaseUpdatesCapability(0))
      client.expectJson(json.ok(0))

      // add atom
      system.eventStream.publish(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          new File("/tmp/foo"),
          versionCalculator.evalVersion("1"),
          Vector(),
          Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestions.atom,
                  Api.SuggestionAction.Add()
                ),
                Vector()
              )
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
        Api.SuggestionsDatabaseModuleUpdateNotification(
          new File("/tmp/foo"),
          versionCalculator.evalVersion("2"),
          Vector(),
          Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestions.atom,
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestions.method,
                      Api.SuggestionAction.Add()
                    ),
                    Vector()
                  )
                )
              )
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
        Api.SuggestionsDatabaseModuleUpdateNotification(
          new File("/tmp/foo"),
          versionCalculator.evalVersion("3"),
          Vector(),
          Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestions.atom,
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestions.method,
                      Api.SuggestionAction.Modify()
                    ),
                    Vector(
                      Tree.Node(
                        Api.SuggestionUpdate(
                          Suggestions.function,
                          Api.SuggestionAction.Add()
                        ),
                        Vector()
                      )
                    )
                  )
                )
              )
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
                "type" : "Add",
                "id" : 3,
                "suggestion" : {
                  "type" : "function",
                  "externalId" : "78d452ce-ed48-48f1-b4f2-b7f45f8dff89",
                  "module" : "Test.Main",
                  "name" : "print",
                  "arguments" : [
                    {
                      "name" : "a",
                      "reprType" : "Any",
                      "isSuspended" : false,
                      "hasDefault" : false,
                      "defaultValue" : null
                    },
                    {
                      "name" : "b",
                      "reprType" : "Any",
                      "isSuspended" : true,
                      "hasDefault" : false,
                      "defaultValue" : null
                    },
                    {
                      "name" : "c",
                      "reprType" : "Any",
                      "isSuspended" : false,
                      "hasDefault" : true,
                      "defaultValue" : "C"
                    }
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
        Api.SuggestionsDatabaseModuleUpdateNotification(
          new File("/tmp/foo"),
          versionCalculator.evalVersion("4"),
          Vector(),
          Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestions.atom,
                  Api.SuggestionAction.Modify()
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestions.method,
                      Api.SuggestionAction.Modify()
                    ),
                    Vector(
                      Tree.Node(
                        Api.SuggestionUpdate(
                          Suggestions.function,
                          Api.SuggestionAction.Modify()
                        ),
                        Vector(
                          Tree.Node(
                            Api.SuggestionUpdate(
                              Suggestions.local,
                              Api.SuggestionAction.Add()
                            ),
                            Vector()
                          )
                        )
                      )
                    )
                  )
                )
              )
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
                    {
                      "name" : "a",
                      "reprType" : "Any",
                      "isSuspended" : false,
                      "hasDefault" : false,
                      "defaultValue" : null
                    },
                    {
                      "name" : "b",
                      "reprType" : "Any",
                      "isSuspended" : true,
                      "hasDefault" : false,
                      "defaultValue" : null
                    },
                    {
                      "name" : "c",
                      "reprType" : "Any",
                      "isSuspended" : false,
                      "hasDefault" : true,
                      "defaultValue" : "C"
                    }
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

      // update items
      system.eventStream.publish(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          new File("/tmp/foo"),
          versionCalculator.evalVersion("5"),
          Vector(),
          Tree.Root(
            Vector(
              Tree.Node(
                Api.SuggestionUpdate(
                  Suggestions.atom,
                  Api.SuggestionAction.Modify(
                    arguments = Some(
                      Seq(
                        Api.SuggestionArgumentAction
                          .Modify(0, reprType = Some("A")),
                        Api.SuggestionArgumentAction
                          .Add(1, Suggestions.function.arguments(1))
                      )
                    )
                  )
                ),
                Vector(
                  Tree.Node(
                    Api.SuggestionUpdate(
                      Suggestions.method,
                      Api.SuggestionAction.Modify()
                    ),
                    Vector(
                      Tree.Node(
                        Api.SuggestionUpdate(
                          Suggestions.function,
                          Api.SuggestionAction.Modify(
                            externalId = Some(None)
                          )
                        ),
                        Vector(
                          Tree.Node(
                            Api.SuggestionUpdate(
                              Suggestions.local,
                              Api.SuggestionAction.Modify(
                                scope = Some(Suggestions.function.scope)
                              )
                            ),
                            Vector()
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      client.expectJson(json"""
        {
          "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Modify",
                "id" : 1,
                "arguments" : [
                  {
                    "type" : "Modify",
                    "index" : 0,
                    "reprType" : {
                      "tag" : "Set",
                      "value" : "A"
                    }
                  },
                  {
                    "type" : "Add",
                    "index" : 1,
                    "argument" : {
                      "name" : "b",
                      "reprType" : "Any",
                      "isSuspended" : true,
                      "hasDefault" : false,
                      "defaultValue" : null
                    }
                  }
                ]
              },
              {
                "type" : "Modify",
                "id" : 3,
                "externalId" : {
                  "tag" : "Remove",
                  "value" : null
                }
              },
              {
                "type" : "Modify",
                "id" : 4,
                "scope" : {
                  "tag" : "Set",
                  "value" : {
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
            "currentVersion" : 7
          }
        }
        """)

      // remove items
      system.eventStream.publish(
        Api.SuggestionsDatabaseModuleUpdateNotification(
          new File("/tmp/foo"),
          versionCalculator.evalVersion("6"),
          Vector(Api.SuggestionsDatabaseAction.Clean(Suggestions.atom.module)),
          Tree.Root(Vector())
        )
      )
      client.expectJson(json"""
        {
          "jsonrpc" : "2.0",
          "method" : "search/suggestionsDatabaseUpdates",
          "params" : {
            "updates" : [
              {
                "type" : "Remove",
                "id" : 1
              },
              {
                "type" : "Remove",
                "id" : 2
              },
              {
                "type" : "Remove",
                "id" : 3
              },
              {
                "type" : "Remove",
                "id" : 4
              }
            ],
            "currentVersion" : 8
          }
        }
        """)

    }
  }

}
