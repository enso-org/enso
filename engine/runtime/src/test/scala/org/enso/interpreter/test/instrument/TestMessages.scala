package org.enso.interpreter.test.instrument

import java.util.UUID

import org.enso.interpreter.runtime.`type`.ConstantsGen
import org.enso.polyglot.runtime.Runtime.Api

/** Helper methods for creating test messages. */
object TestMessages {

  /** Create an update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @return the expression update response
    */
  def update(
    contextId: UUID,
    expressionId: UUID
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        Set(
          Api.ExpressionUpdate(
            expressionId,
            None,
            None,
            Vector(Api.ProfilingInfo.ExecutionTime(0)),
            false,
            true,
            Api.ExpressionUpdate.Payload.Value()
          )
        )
      )
    )

  /** Create an update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param expressionType a type of the expression
    * @return the expression update response
    */
  def update(
    contextId: UUID,
    expressionId: UUID,
    expressionType: String
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        Set(
          Api.ExpressionUpdate(
            expressionId,
            Some(expressionType),
            None,
            Vector(Api.ProfilingInfo.ExecutionTime(0)),
            false,
            true,
            Api.ExpressionUpdate.Payload.Value()
          )
        )
      )
    )

  /** Create an update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param expressionType a type of the expression
    * @param fromCache whether or not the value for this expression came from cache
    * @param typeChanged a flag indicating whether the the type of expression has changed
    * @param methodPointer method pointer
    * @param payload the update payload
    * @return the expression update response
    */
  def update(
    contextId: UUID,
    expressionId: UUID,
    expressionType: String,
    fromCache: Boolean                       = false,
    typeChanged: Boolean                     = true,
    methodPointer: Option[Api.MethodPointer] = None,
    payload: Api.ExpressionUpdate.Payload    = Api.ExpressionUpdate.Payload.Value()
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        Set(
          Api.ExpressionUpdate(
            expressionId,
            Some(expressionType),
            methodPointer,
            Vector(Api.ProfilingInfo.ExecutionTime(0)),
            fromCache,
            typeChanged,
            payload
          )
        )
      )
    )

  /** Create an update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param expressionType a type of the expression
    * @param methodPointer a pointer to the method definition
    * @return the expression update response
    */
  def update(
    contextId: UUID,
    expressionId: UUID,
    expressionType: String,
    methodPointer: Api.MethodPointer
  ): Api.Response =
    update(contextId, expressionId, expressionType, methodPointer, false, true)

  /** Create an update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param expressionType a type of the expression
    * @param methodPointer a pointer to the method definition
    * @param fromCache whether or not the value for this expression came
    * from the cache
    * @param typeChanged a flag indicating whether the the type of expression has changed
    * @return the expression update response
    */
  def update(
    contextId: UUID,
    expressionId: UUID,
    expressionType: String,
    methodPointer: Api.MethodPointer,
    fromCache: Boolean,
    typeChanged: Boolean
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        Set(
          Api.ExpressionUpdate(
            expressionId,
            Some(expressionType),
            Some(methodPointer),
            Vector(Api.ProfilingInfo.ExecutionTime(0)),
            fromCache,
            typeChanged,
            Api.ExpressionUpdate.Payload.Value()
          )
        )
      )
    )

  /** Create an error update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param payload the error payload
    * @return the expression update response
    */
  def error(
    contextId: UUID,
    expressionId: UUID,
    payload: Api.ExpressionUpdate.Payload
  ): Api.Response =
    errorBuilder(
      contextId,
      expressionId,
      None,
      false,
      true,
      payload
    )

  /** Create an error update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param methodPointer a pointer to the method definition
    * @param payload the error payload
    * @return the expression update response
    */
  def error(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Api.MethodPointer,
    payload: Api.ExpressionUpdate.Payload
  ): Api.Response =
    error(
      contextId,
      expressionId,
      methodPointer,
      false,
      true,
      payload
    )

  /** Create an error update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param methodPointer a pointer to the method definition
    * @param fromCache whether or not the value for this expression came
    * from the cache
    * @param typeChanged a flag indicating whether the the type of expression has changed
    * @param payload the error payload
    * @return the expression update response
    */
  def error(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Api.MethodPointer,
    fromCache: Boolean,
    typeChanged: Boolean,
    payload: Api.ExpressionUpdate.Payload
  ): Api.Response =
    errorBuilder(
      contextId,
      expressionId,
      Some(methodPointer),
      fromCache,
      typeChanged,
      payload
    )

  /** Create an error update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param methodPointerOpt a pointer to the method definition
    * @param fromCache whether or not the value for this expression came
    * from the cache
    * @param typeChanged a flag indicating whether the the type of expression has changed
    * @param payload the error payload
    * @return the expression update response
    */
  private def errorBuilder(
    contextId: UUID,
    expressionId: UUID,
    methodPointerOpt: Option[Api.MethodPointer],
    fromCache: Boolean,
    typeChanged: Boolean,
    payload: Api.ExpressionUpdate.Payload
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        Set(
          Api.ExpressionUpdate(
            expressionId,
            Some(ConstantsGen.ERROR),
            methodPointerOpt,
            Vector(Api.ProfilingInfo.ExecutionTime(0)),
            fromCache,
            typeChanged,
            payload
          )
        )
      )
    )

  /** Create a panic update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param payload the error payload
    * @return the expression update response
    */
  def panic(
    contextId: UUID,
    expressionId: UUID,
    payload: Api.ExpressionUpdate.Payload
  ): Api.Response =
    panicBuilder(contextId, expressionId, None, payload, false, true)

  /** Create a panic update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param payload the error payload
    * @param builtin a flag indicating what is the type of Panic (a builtin Panic type or stdlib Panic)
    * @return the expression update response
    */
  def panic(
    contextId: UUID,
    expressionId: UUID,
    payload: Api.ExpressionUpdate.Payload,
    builtin: Boolean
  ): Api.Response =
    panicBuilder(contextId, expressionId, None, payload, builtin, true)

  def panic(
    contextId: UUID,
    expressionId: UUID,
    payload: Api.ExpressionUpdate.Payload,
    builtin: Boolean,
    typeChanged: Boolean
  ): Api.Response =
    panicBuilder(contextId, expressionId, None, payload, builtin, typeChanged)

  /** Create a panic update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param methodPointer a pointer to the method definition
    * @param payload the error payload
    * @param builtin a flag indicating what is the type of Panic (a builtin Panic type or stdlib Panic)
    * @return the expression update response
    */
  def panic(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Api.MethodPointer,
    payload: Api.ExpressionUpdate.Payload,
    builtin: Boolean
  ): Api.Response =
    panicBuilder(
      contextId,
      expressionId,
      Some(methodPointer),
      payload,
      builtin,
      true
    )

  /** Create a panic update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param methodPointer a pointer to the method definition
    * @param payload the error payload
    * @param builtin the type to use
    * @return the expression update response
    */
  def panic(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Api.MethodPointer,
    payload: Api.ExpressionUpdate.Payload,
    builtin: Option[String]
  ): Api.Response =
    panicBuilder(
      contextId,
      expressionId,
      Some(methodPointer),
      payload,
      builtin,
      true
    )

  def panic(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Api.MethodPointer,
    payload: Api.ExpressionUpdate.Payload,
    builtin: Option[String],
    typeChanged: Boolean
  ): Api.Response =
    panicBuilder(
      contextId,
      expressionId,
      Some(methodPointer),
      payload,
      builtin,
      typeChanged
    )

  /** Create a panic update response.
    *
    * @param contextId an identifier of the context
    * @param expressionId an identifier of the expression
    * @param methodPointer a pointer to the method definition
    * @param payload the error payload
    * @param builtin a flag indicating what is the type of Panic (a builtin Panic type or stdlib Panic)
    * @param typeChanged a flag indicating whether the the type of expression has changed
    * @return the expression update response
    */
  private def panicBuilder(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Option[Api.MethodPointer],
    payload: Api.ExpressionUpdate.Payload,
    builtin: Boolean,
    typeChanged: Boolean
  ): Api.Response = panicBuilder(
    contextId,
    expressionId,
    methodPointer,
    payload,
    Some(
      if (builtin) ConstantsGen.PANIC_BUILTIN else ConstantsGen.PANIC
    ),
    typeChanged
  )

  private def panicBuilder(
    contextId: UUID,
    expressionId: UUID,
    methodPointer: Option[Api.MethodPointer],
    payload: Api.ExpressionUpdate.Payload,
    builtin: Option[String],
    typeChanged: Boolean
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        Set(
          Api.ExpressionUpdate(
            expressionId,
            builtin,
            methodPointer,
            Vector(Api.ProfilingInfo.ExecutionTime(0)),
            false,
            typeChanged,
            payload
          )
        )
      )
    )

  /** Create an pending response.
    *
    * @param contextId an identifier of the context
    * @param expressionIds a list of pending expressions
    * @return the expression update response
    */
  def pending(
    contextId: UUID,
    expressionIds: UUID*
  ): Api.Response =
    Api.Response(
      Api.ExpressionUpdates(
        contextId,
        expressionIds.toSet.map { expressionId =>
          Api.ExpressionUpdate(
            expressionId,
            None,
            None,
            Vector(),
            true,
            false,
            Api.ExpressionUpdate.Payload.Pending(None, None)
          )
        }
      )
    )

}
