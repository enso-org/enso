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

      runtimeConnectorProbe.receiveN(1).head match {
        case Api.Request(_, payload: Api.RenameSymbol) =>
          payload.module shouldEqual moduleName
          payload.expressionId shouldEqual expressionId
          payload.newName shouldEqual newName
        case msg =>
          fail(s"Runtime connector received unexpected message: $msg")
      }
      val moduleTextEdits = Api.ModuleTextEdits(
        moduleName,
        Vector(TextEdit(Range(Position(5, 10), Position(5, 16)), newName))
      )
      runtimeConnectorProbe.lastSender ! Api.Response(
        UUID.randomUUID(),
        Api.SymbolRenamed(Vector(moduleTextEdits))
      )

      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": {
              "edits": [ ${ModuleTextEdits(moduleTextEdits)} ]
            }
          }
          """)
    }

  }

}
