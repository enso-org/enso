package org.enso.languageserver.runtime

import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

/** The expression configuration used in the recompute request.
  *
  * @param expressionId the expression identifier
  * @param executionEnvironment the execution environment used to run this expression
  */
case class ExpressionConfig(
  expressionId: UUID,
  executionEnvironment: Option[ExecutionEnvironments.ExecutionEnvironment]
) {

  /** Convert this expression config to the runtime API. */
  def toApi: Api.ExpressionConfig =
    Api.ExpressionConfig(
      expressionId,
      executionEnvironment.map(ExecutionEnvironments.toApi)
    )
}
