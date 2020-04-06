package org.enso.languageserver.websocket

import java.util.UUID

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

    "destroy execution context" in {
      val client = new WsTestClient(address)

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
      val client = new WsTestClient(address)

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
      val client = new WsTestClient(address)

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

    def executionContextCreateResponse(reqId: Int, contextId: Api.ContextId) =
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

    def executionContextDestroyRequest(reqId: Int, contextId: Api.ContextId) =
      json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/destroy",
              "id": $reqId,
              "params": {
                "contextId": $contextId
              }
            }
            """
  }
}
