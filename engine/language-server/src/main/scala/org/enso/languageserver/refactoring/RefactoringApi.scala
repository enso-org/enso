package org.enso.languageserver.refactoring

import org.enso.jsonrpc.{HasParams, HasResult, Method, Unused}

/** The refactoring  JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/develop/docs/language-server/README.md]]
  * for message specifications.
  */
object RefactoringApi {

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

}
