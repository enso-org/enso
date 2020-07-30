package org.enso.languageserver.runtime

import org.enso.languageserver.runtime.ExecutionApi.ExpressionId

/**
  * An update containing information about expression.
  *
  * @param expressionId the id of updated expression
  * @param suggestionId the updated suggestion id
  */
case class ExpressionValueUpdate(expressionId: ExpressionId, suggestionId: Long)
