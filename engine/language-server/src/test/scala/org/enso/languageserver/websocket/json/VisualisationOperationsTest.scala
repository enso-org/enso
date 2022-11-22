package org.enso.languageserver.websocket.json
import java.util.UUID
import io.circe.literal._
import org.enso.languageserver.runtime.{
  MethodPointer,
  VisualisationConfiguration
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model

class VisualisationOperationsTest extends BaseServerTest {

  "executionContext/attachVisualisation" must {

    "return an empty response when the operation succeeds" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualisationRequest(
          1,
          visualisationId,
          expressionId,
          visualisationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualisation(
                  `visualisationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualisationConfig.expression.toApi
            config.visualisationModule shouldBe visualisationConfig.visualisationModule
            config.executionContextId shouldBe visualisationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualisationAttached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "allow attaching method pointer without arguments as a visualisation expression" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client              = getInitialisedWsClient()
      val contextId           = createExecutionContext(client)
      val visualisationModule = "Foo.Bar"
      val visualisationMethod = "baz"
      val visualisationConfig =
        VisualisationConfiguration(
          contextId,
          MethodPointer(
            visualisationModule,
            visualisationModule,
            visualisationMethod
          ),
          Vector()
        )

      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualisationRequest(
          1,
          visualisationId,
          expressionId,
          visualisationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualisation(
                  `visualisationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualisationConfig.expression.toApi
            config.visualisationModule shouldBe visualisationConfig.visualisationModule
            config.executionContextId shouldBe visualisationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualisationAttached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "allow attaching method pointer with arguments as a visualisation expression" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client              = getInitialisedWsClient()
      val contextId           = createExecutionContext(client)
      val visualisationModule = "Foo.Bar"
      val visualisationMethod = "baz"
      val visualisationConfig =
        VisualisationConfiguration(
          contextId,
          MethodPointer(
            visualisationModule,
            visualisationModule,
            visualisationMethod
          ),
          Vector("1", "2", "3")
        )

      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualisationRequest(
          1,
          visualisationId,
          expressionId,
          visualisationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualisation(
                  `visualisationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualisationConfig.expression.toApi
            config.visualisationModule shouldBe visualisationConfig.visualisationModule
            config.executionContextId shouldBe visualisationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualisationAttached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "reply with AccessDenied when context doesn't belong to client" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val contextId       = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualisationRequest(
          1,
          visualisationId,
          expressionId,
          visualisationConfig
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

    "reply with ModuleNotFound error when attaching a visualisation" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar", "_")
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualisationRequest(
          1,
          visualisationId,
          expressionId,
          visualisationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualisation(
                  `visualisationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualisationConfig.expression.toApi
            config.visualisationModule shouldBe visualisationConfig.visualisationModule
            config.executionContextId shouldBe visualisationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.ModuleNotFound(visualisationConfig.visualisationModule)
      )
      client.expectJson(
        ExecutionContextJsonMessages.executionContextModuleNotFound(
          1,
          visualisationConfig.visualisationModule
        )
      )
    }

    "reply with VisualisationExpressionFailed error when attaching a visualisation" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar", "_")
      val expressionFailureMessage = "Method `to_json` could not be found."
      client.send(
        ExecutionContextJsonMessages.executionContextAttachVisualisationRequest(
          1,
          visualisationId,
          expressionId,
          visualisationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.AttachVisualisation(
                  `visualisationId`,
                  `expressionId`,
                  config
                )
              ) =>
            config.expression shouldBe visualisationConfig.expression.toApi
            config.visualisationModule shouldBe visualisationConfig.visualisationModule
            config.executionContextId shouldBe visualisationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualisationExpressionFailed(
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
        s"Evaluation of the visualisation expression failed [$expressionFailureMessage]"
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

  "executionContext/detachVisualisation" must {

    "return an empty response when the operation succeeds" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val contextId       = createExecutionContext(client)
      client.send(
        ExecutionContextJsonMessages.executionContextDetachVisualisationRequest(
          1,
          contextId,
          visualisationId,
          expressionId
        )
      )
      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.DetachVisualisation(
                  `contextId`,
                  `visualisationId`,
                  `expressionId`
                )
              ) =>
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualisationDetached()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "reply with AccessDenied when context doesn't belong to client" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val contextId       = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      client.send(
        ExecutionContextJsonMessages.executionContextDetachVisualisationRequest(
          1,
          contextId,
          visualisationId,
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

  "executionContext/modifyVisualisation" must {

    "return an empty response when the operation succeeds" in {
      val visualisationId = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val contextId       = createExecutionContext(client)
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(
        ExecutionContextJsonMessages.executionContextModifyVisualisationRequest(
          1,
          visualisationId,
          visualisationConfig
        )
      )

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
                requestId,
                Api.ModifyVisualisation(`visualisationId`, config)
              ) =>
            config.expression shouldBe visualisationConfig.expression.toApi
            config.visualisationModule shouldBe visualisationConfig.visualisationModule
            config.executionContextId shouldBe visualisationConfig.executionContextId
            requestId

          case msg =>
            fail(s"Unexpected message: $msg")
        }

      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.VisualisationModified()
      )
      client.expectJson(ExecutionContextJsonMessages.ok(1))
    }

    "reply with AccessDenied when context doesn't belong to client" in {
      val visualisationId = UUID.randomUUID()
      val contextId       = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")

      client.send(
        ExecutionContextJsonMessages.executionContextModifyVisualisationRequest(
          1,
          visualisationId,
          visualisationConfig
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
