package org.enso.languageserver.runtime

import org.enso.languageserver.runtime.ExecutionApi.ExpressionId

/** An update containing information about expression.
  *
  * @param expressionId the id of updated expression
  * @param `type` the updated type of expression
  * @param methodPointer the suggestion id of the updated method pointer
  * @param profilingInfo profiling information about the expression
  * @param fromCache whether or not the expression's value came from the cache
  */
case class ExpressionValueUpdate(
  expressionId: ExpressionId,
  `type`: Option[String],
  methodPointer: Option[Long],
  profilingInfo: Vector[ProfilingInfo],
  fromCache: Boolean
)
