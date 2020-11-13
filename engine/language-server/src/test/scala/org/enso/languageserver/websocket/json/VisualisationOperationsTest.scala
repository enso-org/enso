package org.enso.languageserver.websocket.json
import java.util.UUID

import io.circe.literal._
import org.enso.languageserver.runtime.VisualisationConfiguration
import org.enso.polyglot.runtime.Runtime.Api

import scala.annotation.nowarn

class VisualisationOperationsTest extends BaseServerTest {

  "executionContext/attachVisualisation" must {

    "return an empty response when the operation succeeds" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()

      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(json"""
          {  "jsonrpc": "2.0",
            "method": "executionContext/attachVisualisation",
            "id": 1,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
                  "executionContextId": $contextId,
                  "visualisationModule": ${visualisationConfig.visualisationModule},
                  "expression": ${visualisationConfig.expression}
              }
            }
          }
          """)

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
              requestId,
              Api.AttachVisualisation(`visualisationId`, `expressionId`, config)
              ) =>
            config.expression shouldBe visualisationConfig.expression
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
      client.send(json"""
          {  "jsonrpc": "2.0",
            "method": "executionContext/attachVisualisation",
            "id": 1,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
                  "executionContextId": $contextId,
                  "visualisationModule": ${visualisationConfig.visualisationModule},
                  "expression": ${visualisationConfig.expression}
              }
            }
          }
          """)
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

  "executionContext/detachVisualisation" must {

    "return an empty response when the operation succeeds" in {
      val visualisationId = UUID.randomUUID()
      val expressionId    = UUID.randomUUID()
      val client          = getInitialisedWsClient()
      val contextId       = createExecutionContext(client)
      client.send(json"""
          {  "jsonrpc": "2.0",
            "method": "executionContext/detachVisualisation",
            "id": 1,
            "params": {
              "contextId": $contextId,
              "visualisationId": $visualisationId,
              "expressionId": $expressionId
            }
          }
          """)
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
      @nowarn("cat=unused-locals")
      val visualisationConfig =
        VisualisationConfiguration(contextId, "Foo.Bar.baz", "a=x+y")
      client.send(json"""
          {  "jsonrpc": "2.0",
            "method": "executionContext/detachVisualisation",
            "id": 1,
            "params": {
              "contextId": $contextId,
              "visualisationId": $visualisationId,
              "expressionId": $expressionId
            }
          }
          """)
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
      client.send(json"""
          {  "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualisation",
            "id": 1,
            "params": {
              "visualisationId": $visualisationId,
              "visualisationConfig": {
                  "executionContextId": $contextId,
                  "visualisationModule": ${visualisationConfig.visualisationModule},
                  "expression": ${visualisationConfig.expression}
              }
            }
          }
          """)

      val requestId =
        runtimeConnectorProbe.receiveN(1).head match {
          case Api.Request(
              requestId,
              Api.ModifyVisualisation(`visualisationId`, config)
              ) =>
            config.expression shouldBe visualisationConfig.expression
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
      client.send(json"""
          {  "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualisation",
            "id": 1,
            "params": {
              "visualisationId": $visualisationId,
              "visualisationConfig": {
                  "executionContextId": $contextId,
                  "visualisationModule": ${visualisationConfig.visualisationModule},
                  "expression": ${visualisationConfig.expression}
              }
            }
          }
          """)
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
