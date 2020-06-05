package org.enso.polyglot.debugger.protocol.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.polyglot.debugger.protocol.{
  EvaluationRequest,
  ListBindingsRequest,
  SessionExitRequest
}

object RequestFactory {

  /**
    * Creates EvaluationRequest inside a [[FlatBufferBuilder]].
    *
    * @param expression expression to evaluate
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createEvaluationRequest(
    expression: String
  )(implicit builder: FlatBufferBuilder): Int = {
    val exprOffset = builder.createString(expression)
    EvaluationRequest.createEvaluationRequest(builder, exprOffset)
  }

  /**
    * Creates ListBindingsRequest inside a [[FlatBufferBuilder]].
    *
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createListBindingsRequest()(implicit builder: FlatBufferBuilder): Int = {
    ListBindingsRequest.startListBindingsRequest(builder)
    ListBindingsRequest.endListBindingsRequest(builder)
  }

  /**
    * Creates SessionExitRequest inside a [[FlatBufferBuilder]].
    *
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createSessionExitRequest()(implicit builder: FlatBufferBuilder): Int = {
    SessionExitRequest.startSessionExitRequest(builder)
    SessionExitRequest.endSessionExitRequest(builder)
  }
}
