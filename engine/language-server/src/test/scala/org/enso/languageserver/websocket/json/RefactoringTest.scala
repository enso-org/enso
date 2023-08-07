package org.enso.languageserver.websocket.json

import io.circe.literal._
import io.circe.generic.auto._
import org.enso.languageserver.refactoring.ModuleTextEdits
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.text.editing.model.{Position, Range, TextEdit}

import java.util.UUID

class RefactoringTest extends BaseServerTest {

  "refactoring/renameSymbol" should {

    "return a list of edits after successful renaming" in {
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
      val moduleTextEdits = Api.ModuleTextEdits(
        moduleName,
        Vector(TextEdit(Range(Position(5, 10), Position(5, 16)), newName))
      )
      runtimeConnectorProbe.lastSender ! Api.Response(
        requestId,
        Api.SymbolRenamed(Vector(moduleTextEdits), newName)
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "edits": [ ${ModuleTextEdits(moduleTextEdits)} ],
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
        Api.ExpressionNotFound(expressionId)
      )

      client.expectJson(json"""
                  { "jsonrpc": "2.0",
                    "id": 1,
                    "error": {
                      "code": 2008,
                      "message": ${s"Expression not found by id [$expressionId]"}
                    }
                  }
                  """)
    }
  }

}
