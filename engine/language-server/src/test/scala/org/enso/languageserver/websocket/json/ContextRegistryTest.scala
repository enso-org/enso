package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.languageserver.runtime.{
  TestComponentGroups,
  VisualizationConfiguration
}
import org.enso.languageserver.websocket.json.{
  ExecutionContextJsonMessages => json
}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

class ContextRegistryTest extends BaseServerTest {

  "ContextRegistry" must {

    "create execution context" in {
      val client = getInitialisedWsClient()
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId1) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId1)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId1))

      // check the request with optional parameter
      client.send(
        json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/create",
              "id": 2,
              "params": {
                "contextId": null
              }
            }
            """
      )
      val (requestId2, contextId2) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.CreateContextResponse(contextId2)
      )
      client.expectJson(json.executionContextCreateResponse(2, contextId2))

      // check the request with optional parameter
      val contextId3 = UUID.randomUUID()
      client.send(
        json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/create",
              "id": 3,
              "params": {
                "contextId": $contextId3
              }
            }
            """
      )
      val requestId3 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(`contextId3`)) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId3,
        Api.CreateContextResponse(contextId3)
      )
      client.expectJson(json.executionContextCreateResponse(3, contextId3))

      // check the request with optional parameter
      client.send(
        json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/create",
              "id": 4,
              "params": {}
            }
            """
      )
      val (requestId4, contextId4) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId4,
        Api.CreateContextResponse(contextId4)
      )
      client.expectJson(json.executionContextCreateResponse(4, contextId4))

      // check idempotence
      client.send(json.executionContextCreateRequest(5, contextId1))
      runtimeConnectorProbe.expectNoMessage()
      client.expectJson(json.executionContextCreateResponse(5, contextId1))
    }

    "destroy execution context" in {
      val client = getInitialisedWsClient()
      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // destroy context
      client.send(json.executionContextDestroyRequest(2, contextId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.DestroyContextRequest(`contextId`)) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.DestroyContextResponse(contextId)
      )
      client.expectJson(json.ok(2))
    }

    "reply AccessDenied when destroying context it doesn't hold" in {
      val client = getInitialisedWsClient()
      client.send(json.executionContextDestroyRequest(1, UUID.randomUUID()))
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id" : 1,
            "error" : {
              "code" : 100,
              "message" : "Access denied"
            }
          }
          """)
    }

    "reply ContextNotFound when destroying context that wasn't found" in {
      val client = getInitialisedWsClient()
      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // destroy context
      client.send(json.executionContextDestroyRequest(2, contextId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.DestroyContextRequest(`contextId`)) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.ContextNotExistError(contextId)
      )
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id" : 2,
            "error" : {
              "code" : 2002,
              "message" : "Context not found"
            }
          }
          """)
    }

    "push stack item" in {
      val client = getInitialisedWsClient()
      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // push stack item
      val expressionId = UUID.randomUUID()
      client.send(json.executionContextPushRequest(2, contextId, expressionId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.PushContextRequest(
                  `contextId`,
                  Api.StackItem.LocalCall(`expressionId`)
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.PushContextResponse(contextId)
      )
      client.expectJson(json.ok(2))
    }

    "pop stack item" in {
      val client = getInitialisedWsClient()
      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // push stack item
      val expressionId = UUID.randomUUID()
      client.send(json.executionContextPushRequest(2, contextId, expressionId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.PushContextRequest(
                  `contextId`,
                  Api.StackItem.LocalCall(`expressionId`)
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.PushContextResponse(contextId)
      )
      client.expectJson(json.ok(2))

      // pop stack item
      client.send(json.executionContextPopRequest(3, contextId))
      val requestId3 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.PopContextRequest(`contextId`)) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId3,
        Api.PopContextResponse(contextId)
      )
      client.expectJson(json.ok(3))
    }

    "return EmptyStackError when popping empty stack" in {
      val client = getInitialisedWsClient()
      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // pop stack item
      client.send(json.executionContextPopRequest(2, contextId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.PopContextRequest(`contextId`)) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.EmptyStackError(contextId)
      )
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id" : 2,
            "error" : {
              "code" : 2003,
              "message" : "Stack is empty"
            }
          }
          """)
    }

    "return InvalidStackItemError when pushing invalid item to stack" in {
      val client = getInitialisedWsClient()
      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // push invalid item
      val expressionId = UUID.randomUUID()
      client.send(json.executionContextPushRequest(2, contextId, expressionId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.PushContextRequest(
                  `contextId`,
                  Api.StackItem.LocalCall(`expressionId`)
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.InvalidStackItemError(contextId)
      )
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id" : 2,
            "error" : {
              "code" : 2004,
              "message" : "Invalid stack item"
            }
          }
          """)
    }

    "recompute all expressions" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // push stack item
      val expressionId = UUID.randomUUID()
      client.send(json.executionContextPushRequest(2, contextId, expressionId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.PushContextRequest(
                  `contextId`,
                  Api.StackItem.LocalCall(`expressionId`)
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.PushContextResponse(contextId)
      )
      client.expectJson(json.ok(2))

      // recompute
      client.send(
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/recompute",
            "id": 3,
            "params": {
              "contextId": $contextId,
              "invalidatedExpressions": "all"
            }
          }
          """
      )
      val requestId3 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.RecomputeContextRequest(
                  `contextId`,
                  Some(Api.InvalidatedExpressions.All()),
                  None
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId3,
        Api.RecomputeContextResponse(contextId)
      )
      client.expectJson(json.ok(3))
    }

    "recompute a list of expressions" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // push stack item
      val expressionId = UUID.randomUUID()
      client.send(json.executionContextPushRequest(2, contextId, expressionId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.PushContextRequest(
                  `contextId`,
                  Api.StackItem.LocalCall(`expressionId`)
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.PushContextResponse(contextId)
      )
      client.expectJson(json.ok(2))

      // recompute
      client.send(
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/recompute",
            "id": 3,
            "params": {
              "contextId": $contextId,
              "invalidatedExpressions": [ $expressionId ]
            }
          }
          """
      )
      val requestId3 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.RecomputeContextRequest(
                  `contextId`,
                  Some(
                    Api.InvalidatedExpressions.Expressions(
                      Vector(`expressionId`)
                    )
                  ),
                  None
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId3,
        Api.RecomputeContextResponse(contextId)
      )
      client.expectJson(json.ok(3))
    }

    "set execution environment" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // push stack item
      val expressionId = UUID.randomUUID()
      client.send(json.executionContextPushRequest(2, contextId, expressionId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.PushContextRequest(
                  `contextId`,
                  Api.StackItem.LocalCall(`expressionId`)
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.PushContextResponse(contextId)
      )
      client.expectJson(json.ok(2))

      // recompute
      client.send(
        json"""
              { "jsonrpc": "2.0",
                "method": "executionContext/setExecutionEnvironment",
                "id": 3,
                "params": {
                  "contextId": $contextId,
                  "executionEnvironment": "Live"
                }
              }
              """
      )
      val requestId3 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.SetExecutionEnvironmentRequest(
                  `contextId`,
                  Api.ExecutionEnvironment.Live()
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId3,
        Api.SetExecutionEnvironmentResponse(contextId)
      )
      client.expectJson(json.ok(3))
    }

    "successfully execute expression" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // attach visualization
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      client.send(
        json.executionContextExecuteExpressionRequest(
          2,
          contextId,
          visualizationId,
          expressionId,
          "expression"
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.ExecuteExpression(
                  `contextId`,
                  `visualizationId`,
                  `expressionId`,
                  _
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.VisualizationAttached()
      )
      client.expectJson(json.ok(2))
    }

    "successfully attach visualization" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // attach visualization
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val config =
        VisualizationConfiguration(contextId, "Test.Main", ".to_json.to_text")
      client.send(
        json.executionContextAttachVisualizationRequest(
          2,
          visualizationId,
          expressionId,
          config
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  _
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.VisualizationAttached()
      )
      client.expectJson(json.ok(2))
    }

    "return ModuleNotFound error when attaching visualization" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // attach visualization
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val config =
        VisualizationConfiguration(contextId, "Test.Main", ".to_json.to_text")
      client.send(
        json.executionContextAttachVisualizationRequest(
          2,
          visualizationId,
          expressionId,
          config
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  _
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.ModuleNotFound(config.visualizationModule)
      )
      client.expectJson(
        json.executionContextModuleNotFound(
          2,
          config.visualizationModule
        )
      )
    }

    "return VisualizationExpressionFailed error when attaching visualization" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // attach visualization
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val config =
        VisualizationConfiguration(contextId, "Test.Main", ".to_json.to_text")
      val expressionFailureMessage = "Method `to_json` could not be found."
      client.send(
        json.executionContextAttachVisualizationRequest(
          2,
          visualizationId,
          expressionId,
          config
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  _
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.VisualizationExpressionFailed(
          Api.VisualizationContext(visualizationId, contextId, expressionId),
          expressionFailureMessage,
          None
        )
      )
      client.expectJson(
        json.executionContextVisualizationExpressionFailed(
          2,
          expressionFailureMessage
        )
      )
    }

    "successfully detach visualization" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // detach visualization
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      client.send(
        json.executionContextDetachVisualizationRequest(
          2,
          contextId,
          visualizationId,
          expressionId
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.DetachVisualization(
                  `contextId`,
                  `visualizationId`,
                  `expressionId`
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.VisualizationDetached()
      )
      client.expectJson(json.ok(2))
    }

    "successfully modify visualization" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // modify visualization
      val visualizationId = UUID.randomUUID()
      val config =
        VisualizationConfiguration(contextId, "Test.Main", ".to_json.to_text")
      client.send(
        json.executionContextModifyVisualizationRequest(
          2,
          visualizationId,
          config
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.ModifyVisualization(
                  `visualizationId`,
                  _
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.VisualizationModified()
      )
      client.expectJson(json.ok(2))
    }

    "return VisualizationNotFound error when modifying visualization" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // modify visualization
      val visualizationId = UUID.randomUUID()
      val config =
        VisualizationConfiguration(contextId, "Test.Main", ".to_json.to_text")
      client.send(
        json.executionContextModifyVisualizationRequest(
          2,
          visualizationId,
          config
        )
      )
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.ModifyVisualization(
                  `visualizationId`,
                  _
                )
              ) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.VisualizationNotFound()
      )
      client.expectJson(json.executionContextVisualizationNotFound(2))
    }

    "get component groups" in {
      val client = getInitialisedWsClient()

      // create context
      client.send(json.executionContextCreateRequest(1))
      val (requestId1, contextId) =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.CreateContextRequest(contextId)) =>
            (requestId, contextId)
          case msg =>
            fail(s"Unexpected message: $msg")
        }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId1,
        Api.CreateContextResponse(contextId)
      )
      client.expectJson(json.executionContextCreateResponse(1, contextId))

      // get component groups
      client.send(json.executionContextGetComponentGroupsRequest(2, contextId))
      val requestId2 =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(requestId, Api.GetComponentGroupsRequest()) =>
            requestId
          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId2,
        Api.GetComponentGroupsResponse(
          TestComponentGroups.standardBase.toVector
        )
      )
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": {
              "componentGroups": [
                {
                  "library" : "Standard.Base",
                  "name" : "Input",
                  "exports" : [
                    {
                      "name" : "Standard.Base.File.new"
                    },
                    {
                      "name" : "Standard.Database.Connection.Database.connect"
                    }
                  ]
                }
              ]
            }
          }
          """)
    }

  }

}
