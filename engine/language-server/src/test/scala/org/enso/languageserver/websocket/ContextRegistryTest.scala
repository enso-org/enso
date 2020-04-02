package org.enso.languageserver.websocket

import io.circe.literal._
import org.enso.polyglot.runtime.Runtime.Api

class ContextRegistryTest extends BaseServerTest {

  "ContextRegistry" must {

    "create execution context" in {
      val client = new WsTestClient(address)

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
    }

  }

  object json {

    def ok(reqId: Int) =
      json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "result": null
          }
          """

    def executionContextCreateRequest(reqId: Int) =
      json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/create",
              "id": $reqId,
              "params": null
            }
            """

    def executionContextCreateResponse(
      reqId: Int,
      contextId: Api.ContextId
    ) =
      json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "result" : {
              "canModify" : {
                "method" : "canModify",
                "registerOptions" : {
                  "contextId" : $contextId
                }
              },
              "receivesEvents" : {
                "method" : "receivesEvents",
                "registerOptions" : {
                  "contextId" : $contextId
                }
              }
            }
          }
          """
  }
}
