package org.enso.languageserver.refactoring

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

import java.util.UUID

/** The refactoring  JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object RefactoringApi {

  type ExpressionId = UUID

  case object RenameProject extends Method("refactoring/renameProject") {

    case class Params(namespace: String, oldName: String, newName: String)

    implicit val hasParams: HasParams.Aux[this.type, RenameProject.Params] =
      new HasParams[this.type] {
        type Params = RenameProject.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, Unused.type] =
      new HasResult[this.type] {
        type Result = Unused.type
      }
  }

  case object RenameSymbol extends Method("refactoring/renameSymbol") {

    case class Params(
      module: String,
      expressionId: ExpressionId,
      newName: String
    )

    case class Result(edits: Seq[ModuleTextEdits], newName: String)

    implicit val hasParams: HasParams.Aux[this.type, RenameSymbol.Params] =
      new HasParams[this.type] {
        type Params = RenameSymbol.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, RenameSymbol.Result] =
      new HasResult[this.type] {
        type Result = RenameSymbol.Result
      }
  }
}
