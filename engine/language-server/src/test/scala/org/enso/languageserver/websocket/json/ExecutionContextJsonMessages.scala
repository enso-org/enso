package org.enso.languageserver.websocket.json

import org.enso.polyglot.runtime.Runtime.Api
import io.circe.literal._
import org.enso.languageserver.runtime.{
  VisualizationConfiguration,
  VisualizationExpression
}

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

  def executionContextAttachVisualizationRequest(
    reqId: Int,
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId,
    configuration: VisualizationConfiguration
  ) = {
    configuration.expression match {
      case VisualizationExpression.Text(module, expression) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/attachVisualization",
            "id": $reqId,
            "params": {
              "visualizationId": $visualizationId,
              "expressionId": $expressionId,
              "visualizationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "visualizationModule": $module,
                "expression": $expression
              }
            }
          }
          """
      case VisualizationExpression.ModuleMethod(methodPointer, Vector()) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/attachVisualization",
            "id": $reqId,
            "params": {
              "visualizationId": $visualizationId,
              "expressionId": $expressionId,
              "visualizationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "visualizationModule": ${configuration.visualizationModule},
                "expression": {
                  "module": ${methodPointer.module},
                  "definedOnType": ${methodPointer.definedOnType},
                  "name": ${methodPointer.name}
                }
              }
            }
          }
          """
      case VisualizationExpression.ModuleMethod(methodPointer, arguments) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/attachVisualization",
            "id": $reqId,
            "params": {
              "visualizationId": $visualizationId,
              "expressionId": $expressionId,
              "visualizationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "visualizationModule": ${methodPointer.module},
                "expression": {
                  "module": ${methodPointer.module},
                  "definedOnType": ${methodPointer.definedOnType},
                  "name": ${methodPointer.name}
                },
                "positionalArgumentsExpressions": $arguments
              }
            }
          }
          """
    }
  }

  def executionContextModuleNotFound(
    reqId: Int,
    module: String
  ) = {
    val errorMessage =
      s"Module not found [$module]"
    json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "error": {
              "code": 2005,
              "message": $errorMessage
            }
          }
          """
  }

  def executionContextVisualizationNotFound(reqId: Int) =
    json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "error": {
              "code": 2006,
              "message": "Visualization not found"
            }
          }
          """

  def executionContextVisualizationExpressionFailed(
    reqId: Int,
    message: String
  ) = {
    val errorMessage =
      s"Evaluation of the visualization expression failed [$message]"
    json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "error": {
              "code": 2007,
              "message": $errorMessage
            }
          }
          """
  }

  def executionContextDetachVisualizationRequest(
    reqId: Int,
    contextId: Api.ContextId,
    visualizationId: Api.VisualizationId,
    expressionId: Api.ExpressionId
  ) =
    json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/detachVisualization",
            "id": $reqId,
            "params": {
              "contextId": $contextId,
              "visualizationId": $visualizationId,
              "expressionId": $expressionId
            }
          }
          """

  def executionContextModifyVisualizationRequest(
    reqId: Int,
    visualizationId: Api.VisualizationId,
    configuration: VisualizationConfiguration
  ) = {
    configuration.expression match {
      case VisualizationExpression.Text(module, expression) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualization",
            "id": $reqId,
            "params": {
              "visualizationId": $visualizationId,
              "visualizationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "visualizationModule": $module,
                "expression": $expression
              }
            }
          }
          """
      case VisualizationExpression.ModuleMethod(methodPointer, Vector()) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualization",
            "id": $reqId,
            "params": {
              "visualizationId": $visualizationId,
              "visualizationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "expression": {
                  "module": ${methodPointer.module},
                  "definedOnType": ${methodPointer.definedOnType},
                  "name": ${methodPointer.name}
                }
              }
            }
          }
          """
      case VisualizationExpression.ModuleMethod(methodPointer, arguments) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualization",
            "id": $reqId,
            "params": {
              "visualizationId": $visualizationId,
              "visualizationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "expression": {
                  "module": ${methodPointer.module},
                  "definedOnType": ${methodPointer.definedOnType},
                  "name": ${methodPointer.name}
                },
                "positionalArgumentsExpressions": $arguments
              }
            }
          }
          """
    }
  }

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
