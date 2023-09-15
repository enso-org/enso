package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

class RefactoringTest extends BaseServerTest {

  "refactoring/renameProject" should {

    "return ok response after a successful renaming" in {
      val client = getInitialisedWsClient()

      val namespace = "local"
      val oldName   = "Unnamed"
      val newName   = "Project1"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameProject",
            "id": 1,
            "params": {
              "namespace": $namespace,
              "oldName": $oldName,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameProject) =>
          payload.namespace shouldEqual namespace
          payload.oldName shouldEqual oldName
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.ProjectRenamed(oldName, newName, newName)
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/projectRenamed",
            "params": {
              "oldNormalizedName": $oldName,
              "newNormalizedName": $newName,
              "newName": $newName
            }
          }
          """)
    }

    "reply with ProjectRenameFailed in case of failures" in {
      val client = getInitialisedWsClient()

      val namespace = "local"
      val oldName   = "Unnamed"
      val newName   = "Project1"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameProject",
            "id": 1,
            "params": {
              "namespace": $namespace,
              "oldName": $oldName,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameProject) =>
          payload.namespace shouldEqual namespace
          payload.oldName shouldEqual oldName
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.ProjectRenameFailed(oldName, newName)
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 9004,
              "message": ${s"Project rename failed [$oldName, $newName]"}
            }
          }
          """)
    }

  }

  "refactoring/renameSymbol" should {

    "return ok response after a successful renaming" in {
      val client = getInitialisedWsClient()

      val moduleName   = "local.Unnamed.Main"
      val expressionId = new UUID(0, 1)
      val newName      = "bar"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameSymbol",
            "id": 1,
            "params": {
              "module": $moduleName,
              "expressionId": $expressionId,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameSymbol) =>
          payload.module shouldEqual moduleName
          payload.expressionId shouldEqual expressionId
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.SymbolRenamed(newName)
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "newName": $newName
            }
          }
          """)
    }

    "reply with ModuleNotFound error when the requested module not found" in {
      val client = getInitialisedWsClient()

      val moduleName   = "local.Unnamed.Foo"
      val expressionId = new UUID(0, 1)
      val newName      = "bar"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameSymbol",
            "id": 1,
            "params": {
              "module": $moduleName,
              "expressionId": $expressionId,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameSymbol) =>
          payload.module shouldEqual moduleName
          payload.expressionId shouldEqual expressionId
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.ModuleNotFound(moduleName)
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 2005,
              "message": ${s"Module not found [$moduleName]"}
            }
          }
          """)
    }

    "reply with ExpressionNotFound error when the requested expression not found" in {
      val client = getInitialisedWsClient()

      val moduleName   = "local.Unnamed.Main"
      val expressionId = new UUID(0, 1)
      val newName      = "bar"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameSymbol",
            "id": 1,
            "params": {
              "module": $moduleName,
              "expressionId": $expressionId,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameSymbol) =>
          payload.module shouldEqual moduleName
          payload.expressionId shouldEqual expressionId
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.ExpressionNotFound(expressionId)
        )
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 9001,
              "message": ${s"Expression not found by id [$expressionId]"}
            }
          }
          """)
    }

    "reply with FailedToApplyEdits error when failed to apply edits" in {
      val client = getInitialisedWsClient()

      val moduleName   = "local.Unnamed.Main"
      val expressionId = new UUID(0, 1)
      val newName      = "bar"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameSymbol",
            "id": 1,
            "params": {
              "module": $moduleName,
              "expressionId": $expressionId,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameSymbol) =>
          payload.module shouldEqual moduleName
          payload.expressionId shouldEqual expressionId
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.FailedToApplyEdits(moduleName)
        )
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 9002,
              "message": ${s"Failed to apply edits to module [$moduleName]"}
            }
          }
          """)
    }

    "reply with RefactoringNotSupported error when renaming unsupported expression" in {
      val client = getInitialisedWsClient()

      val moduleName   = "local.Unnamed.Main"
      val expressionId = new UUID(0, 1)
      val newName      = "bar"

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "refactoring/renameSymbol",
            "id": 1,
            "params": {
              "module": $moduleName,
              "expressionId": $expressionId,
              "newName": $newName
            }
          }
          """)

      val requestId = runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(requestId, payload: Api.RenameSymbol) =>
          payload.module shouldEqual moduleName
          payload.expressionId shouldEqual expressionId
          payload.newName shouldEqual newName
          requestId
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.SymbolRenameFailed(
          Api.SymbolRenameFailed.OperationNotSupported(expressionId)
        )
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 9003,
              "message": ${s"Refactoring not supported for expression [$expressionId]"}
            }
          }
          """)
    }
  }

}
