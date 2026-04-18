package org.enso.languageserver.websocket.json

import org.enso.polyglot.runtime.Runtime.Api
import io.circe.literal._

import java.util.UUID

object ExecutionContextJsonMessages {

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

  def executionContextCreateRequest(reqId: Int, contextId: Api.ContextId) =
    json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/create",
              "id": $reqId,
              "params": {
                "contextId": $contextId
              }
            }
            """

  def executionContextCreateResponse(reqId: Int, contextId: Api.ContextId) =
    json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "result" : {
              "contextId" : $contextId,
              "canModify" : {
                "method" : "executionContext/canModify",
                "registerOptions" : {
                  "contextId" : $contextId
                }
              },
              "receivesUpdates" : {
                "method" : "executionContext/receivesUpdates",
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
              "stackItem": ${localCall(expressionId)}
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

  def executionContextExecuteExpressionRequest(
    reqId: Int,
    executionContextId: UUID,
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    expression: String
  ) =
    json"""
      { "jsonrpc": "2.0",
        "method": "executionContext/executeExpression",
        "id": $reqId,
        "params": {
          "executionContextId": $executionContextId,
          "visualizationId": $visualizationId,
          "expressionId": $expressionId,
          "expression": $expression
        }
      }
      """


  def executionContextGetComponentGroupsRequest(
    reqId: Int,
    contextId: Api.ContextId
  ) =
    json"""
            { "jsonrpc": "2.0",
              "method": "executionContext/getComponentGroups",
              "id": $reqId,
              "params": {
                "contextId": $contextId
              }
            }
            """

}
