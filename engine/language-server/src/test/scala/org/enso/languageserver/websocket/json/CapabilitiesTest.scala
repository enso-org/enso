package org.enso.languageserver.websocket.json

import io.circe.literal._
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

class CapabilitiesTest extends BaseServerTest {

  "capability/acquire" must {

    "return an error response to the client when requesting it for `executionContext/canModify`" in {
      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      client.send(json"""
        { "jsonrpc": "2.0",
          "method": "capability/acquire",
          "id": 1,
          "params": {
            "method" : "executionContext/canModify",
            "registerOptions": {
             "contextId" : $contextId
            }
          }
        }
        """)
      val response = parse(client.expectMessage()).rightValue.asObject.value
      response("jsonrpc") shouldEqual Some("2.0".asJson)
      response("id") shouldEqual Some(1.asJson)
      val err = response("error").value.asObject.value
      err("message") shouldEqual Some("Service error".asJson)
    }
  }

  "capability/release" must {

    "return an error response to the client when requesting it for `executionContext/canModify`" in {
      val client    = getInitialisedWsClient()
      val contextId = createExecutionContext(client)
      client.send(json"""
        { "jsonrpc": "2.0",
          "method": "capability/release",
          "id": 1,
          "params": {
            "method" : "executionContext/canModify",
            "registerOptions": {
             "contextId" : $contextId
            }
          }
        }
        """)
      val response = parse(client.expectMessage()).rightValue.asObject.value
      response("jsonrpc") shouldEqual Some("2.0".asJson)
      response("id") shouldEqual Some(1.asJson)
      val err = response("error").value.asObject.value
      err("message") shouldEqual Some("Service error".asJson)
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
