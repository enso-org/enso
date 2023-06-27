package org.enso.languageserver.websocket.json
import java.util.UUID
import io.circe.literal._
import org.enso.languageserver.runtime.{
  MethodPointer,
  VisualizationConfiguration
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model

class VisualizationOperationsTest extends BaseServerTest {

  "executionContext/attachVisualization" must {

    "return an empty response when the operation succeeds" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualizationConfig =
        VisualizationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualizationRequest(
          1,
          visualizationId,
          expressionId,
          visualizationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualizationConfig.expression.toApi
            config.visualizationModule shouldBe visualizationConfig.visualizationModule
            config.executionContextId shouldBe visualizationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualizationAttached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "allow attaching method pointer without arguments as a visualization expression" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client              = getInitialisedWsClient()
      val contextId           = createExecutionContext(client)
      val visualizationModule = "Foo.Bar"
      val visualizationMethod = "baz"
      val visualizationConfig =
        VisualizationConfiguration(
          contextId,
          MethodPointer(
            visualizationModule,
            visualizationModule,
            visualizationMethod
          ),
          Vector()
        )

      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualizationRequest(
          1,
          visualizationId,
          expressionId,
          visualizationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualizationConfig.expression.toApi
            config.visualizationModule shouldBe visualizationConfig.visualizationModule
            config.executionContextId shouldBe visualizationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualizationAttached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "allow attaching method pointer with arguments as a visualization expression" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client              = getInitialisedWsClient()
      val contextId           = createExecutionContext(client)
      val visualizationModule = "Foo.Bar"
      val visualizationMethod = "baz"
      val visualizationConfig =
        VisualizationConfiguration(
          contextId,
          MethodPointer(
            visualizationModule,
            visualizationModule,
            visualizationMethod
          ),
          Vector("1", "2", "3")
        )

      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualizationRequest(
          1,
          visualizationId,
          expressionId,
          visualizationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualizationConfig.expression.toApi
            config.visualizationModule shouldBe visualizationConfig.visualizationModule
            config.executionContextId shouldBe visualizationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualizationAttached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "reply with AccessDenied when context doesn't belong to client" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val contextId       = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val visualizationConfig =
        VisualizationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualizationRequest(
          1,
          visualizationId,
          expressionId,
          visualizationConfig
        )
      )
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

    "reply with ModuleNotFound error when attaching a visualization" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualizationConfig =
        VisualizationConfiguration(contextId, "Foo.Bar", "_")
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualizationRequest(
          1,
          visualizationId,
          expressionId,
          visualizationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualizationConfig.expression.toApi
            config.visualizationModule shouldBe visualizationConfig.visualizationModule
            config.executionContextId shouldBe visualizationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.ModuleNotFound(visualizationConfig.visualizationModule)
      )
      client.expectJson(
        ExecutionContextJsonMessages.executionContextModuleNotFound(
          1,
          visualizationConfig.visualizationModule
        )
      )
    }

    "reply with VisualizationExpressionFailed error when attaching a visualization" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualizationConfig =
        VisualizationConfiguration(contextId, "Foo.Bar", "_")
      val expressionFailureMessage = "Method `to_json` could not be found."
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualizationRequest(
          1,
          visualizationId,
          expressionId,
          visualizationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualization(
                  `visualizationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualizationConfig.expression.toApi
            config.visualizationModule shouldBe visualizationConfig.visualizationModule
            config.executionContextId shouldBe visualizationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualizationExpressionFailed(
          expressionFailureMessage,
          Some(
            Api.ExecutionResult.Diagnostic.error(
              expressionFailureMessage,
              location = Some(
                model.Range(model.Position(0, 0), model.Position(0, 15))
              ),
              expressionId = Some(expressionId)
            )
          )
        )
      )
      val errorMessage =
        s"Evaluation of the visualization expression failed [$expressionFailureMessage]"
      client.expectJson(
        json"""
          { "jsonrpc" : "2.0",
            "id" : 1,
            "error" : {
              "code" : 2007,
              "message" : $errorMessage,
              "payload": {
                "kind" : "Error",
                "message" : $expressionFailureMessage,
                "path" : null,
                "location" : {
                  "start" : {
                    "line" : 0,
                    "character" : 0
                  },
                  "end" : {
                    "line" : 0,
                    "character" : 15
                  }
                },
                "expressionId" : $expressionId,
                "stack" : []
              }
            }
          }
          """
      )
    }

  }

  "executionContext/detachVisualization" must {

    "return an empty response when the operation succeeds" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val contextId       = createExecutionContext(client)
      client.send(
        ExecutionContextJsonMessages.executionContextDetachVisualizationRequest(
          1,
          contextId,
          visualizationId,
          expressionId
        )
      )
      val requestId =
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
        requestId,
        Api.VisualizationDetached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "reply with AccessDenied when context doesn't belong to client" in {
      val visualizationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val contextId       = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      client.send(
        ExecutionContextJsonMessages.executionContextDetachVisualizationRequest(
          1,
          contextId,
          visualizationId,
          expressionId
        )
      )
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

  }

  "executionContext/modifyVisualization" must {

    "return an empty response when the operation succeeds" in {
      val visualizationId = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val contextId       = createExecutionContext(client)
      val visualizationConfig =
        VisualizationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(
        ExecutionContextJsonMessages.executionContextModifyVisualizationRequest(
          1,
          visualizationId,
          visualizationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.ModifyVisualization(`visualizationId`, config)
              ) =>
            config.expression shouldBe visualizationConfig.expression.toApi
            config.visualizationModule shouldBe visualizationConfig.visualizationModule
            config.executionContextId shouldBe visualizationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualizationModified()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "reply with AccessDenied when context doesn't belong to client" in {
      val visualizationId = UUID.randomUUID()
      val contextId       = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val visualizationConfig =
        VisualizationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")

      client.send(
        ExecutionContextJsonMessages.executionContextModifyVisualizationRequest(
          1,
          visualizationId,
          visualizationConfig
        )
      )
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
  }

  private def createExecutionContext(client: WsTestClient): UUID = {
    client.send(ExecutionContextJsonMessages.executionContextCreateRequest(0))
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
    client.expectJson(
      ExecutionContextJsonMessages.executionContextCreateResponse(0, contextId)
    )
    contextId
  }

}
