package org.enso.languageserver.websocket.json

import org.enso.polyglot.runtime.Runtime.Api
import io.circe.literal._
import org.enso.languageserver.runtime.{
  VisualisationConfiguration,
  VisualisationExpression
}

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
    visualisationId: Api.VisualisationId,
    expressionId: Api.ExpressionId,
    configuration: VisualisationConfiguration
  ) =
    configuration.expression match {
      case VisualisationExpression.Text(module, expression) =>
        json"""
              { "jsonrpc": "2.0",
                "method": "executionContext/executeExpression",
                "id": $reqId,
                "params": {
                  "visualisationId": $visualisationId,
                  "expressionId": $expressionId,
                  "visualisationConfig": {
                    "executionContextId": ${configuration.executionContextId},
                    "visualisationModule": $module,
                    "expression": $expression
                  }
                }
              }
              """
      case VisualisationExpression.ModuleMethod(methodPointer, Vector()) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/executeExpression",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
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
      case VisualisationExpression.ModuleMethod(methodPointer, arguments) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/executeExpression",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
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

  def executionContextAttachVisualisationRequest(
    reqId: Int,
    visualisationId: Api.VisualisationId,
    expressionId: Api.ExpressionId,
    configuration: VisualisationConfiguration
  ) = {
    configuration.expression match {
      case VisualisationExpression.Text(module, expression) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/attachVisualisation",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "visualisationModule": $module,
                "expression": $expression
              }
            }
          }
          """
      case VisualisationExpression.ModuleMethod(methodPointer, Vector()) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/attachVisualisation",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
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
      case VisualisationExpression.ModuleMethod(methodPointer, arguments) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/attachVisualisation",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "expressionId": $expressionId,
              "visualisationConfig": {
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

  def executionContextVisualisationNotFound(reqId: Int) =
    json"""
          { "jsonrpc": "2.0",
            "id": $reqId,
            "error": {
              "code": 2006,
              "message": "Visualisation not found"
            }
          }
          """

  def executionContextVisualisationExpressionFailed(
    reqId: Int,
    message: String
  ) = {
    val errorMessage =
      s"Evaluation of the visualisation expression failed [$message]"
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

  def executionContextDetachVisualisationRequest(
    reqId: Int,
    contextId: Api.ContextId,
    visualisationId: Api.VisualisationId,
    expressionId: Api.ExpressionId
  ) =
    json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/detachVisualisation",
            "id": $reqId,
            "params": {
              "contextId": $contextId,
              "visualisationId": $visualisationId,
              "expressionId": $expressionId
            }
          }
          """

  def executionContextModifyVisualisationRequest(
    reqId: Int,
    visualisationId: Api.VisualisationId,
    configuration: VisualisationConfiguration
  ) = {
    configuration.expression match {
      case VisualisationExpression.Text(module, expression) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualisation",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "visualisationConfig": {
                "executionContextId": ${configuration.executionContextId},
                "visualisationModule": $module,
                "expression": $expression
              }
            }
          }
          """
      case VisualisationExpression.ModuleMethod(methodPointer, Vector()) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualisation",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "visualisationConfig": {
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
      case VisualisationExpression.ModuleMethod(methodPointer, arguments) =>
        json"""
          { "jsonrpc": "2.0",
            "method": "executionContext/modifyVisualisation",
            "id": $reqId,
            "params": {
              "visualisationId": $visualisationId,
              "visualisationConfig": {
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
