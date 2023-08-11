package org.enso.languageserver.refactoring

import org.enso.jsonrpc.Error
import org.enso.polyglot.runtime.Runtime.Api

object RenameFailureMapper {

  /** Maps refactoring error into JSON RPC error.
    *
    * @param error refactoring error
    * @return JSON RPC error
    */
  def mapFailure(error: Api.SymbolRenameFailed.Error): Error =
    error match {
      case error: Api.SymbolRenameFailed.ExpressionNotFound =>
        RefactoringApi.ExpressionNotFoundError(error.expressionId)

      case error: Api.SymbolRenameFailed.FailedToApplyEdits =>
        RefactoringApi.FailedToApplyEdits(error.module)

      case error: Api.SymbolRenameFailed.OperationNotSupported =>
        RefactoringApi.RefactoringNotSupported(error.expressionId)
    }

}
