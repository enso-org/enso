package org.enso.languageserver.websocket

import java.nio.file.Paths
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

    "push stack item" in {
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

    "send notifications" in {
      val client = new WsTestClient(address)

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

      // notify
      val update = Api.ExpressionValueUpdate(
        expressionId   = UUID.randomUUID(),
        expressionType = Some("ExpressionType"),
        shortValue     = Some("ShortValue"),
        methodCall = Some(
          Api.MethodPointer(
            file          = testContentRoot,
            definedOnType = "DefinedOnType",
            name          = "Name"
          )
        )
      )
      val invalidPathUpdate = Api.ExpressionValueUpdate(
        expressionId   = UUID.randomUUID(),
        expressionType = None,
        shortValue     = None,
        methodCall = Some(
          Api.MethodPointer(Paths.get("/invalid"), "Invalid", "Invalid")
        )
      )
      system.eventStream.publish(
        Api.ExpressionValuesComputed(
          contextId,
          Vector(update, invalidPathUpdate)
        )
      )
      client.expectJson(json"""
          { "jsonrpc" : "2.0",
            "method" : "executionContext/expressionValuesComputed",
            "params" : {
              "contextId" : $contextId,
              "updates" : [
                {
                  "id" : ${update.expressionId},
                  "type" : "ExpressionType",
                  "shortValue" : "ShortValue",
                  "methodCall" : {
                    "file" : {
                      "rootId" : $testContentRootId,
                      "segments" : [
                      ]
                    },
                    "definedOnType" : "DefinedOnType",
                    "name" : "Name"
                  }
                }
              ]
            }
          }
      """)
    }
  }

  object json {

    def localCall(expressionId: Api.ExpressionId) =
      json"""
          { "type": "LocalCall",
            "expressionId": $expressionId
          }
          """

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

    def executionContextPushRequest(
      reqId: Int,
      contextId: Api.ContextId,
      expressionId: Api.ExpressionId
    ) =
      json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/push",
            "id": $reqId,
            "params": {
              "contextId": $contextId,
              "stackItem": ${json.localCall(expressionId)}
            }
          }
          """

    def executionContextPopRequest(
      reqId: Int,
      contextId: Api.ContextId
    ) =
      json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/pop",
            "id": $reqId,
            "params": {
              "contextId": $contextId
            }
          }
          """
  }
}
