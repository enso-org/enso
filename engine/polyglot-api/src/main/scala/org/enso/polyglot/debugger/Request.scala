package org.enso.polyglot.debugger

/**
  * Represents a deserialized request that is received at the debugger.
  *
  * It is a separate class from BinaryRequest to encapsulate Flatbuffers union
  * handling logic in a type-safe way.
  */
sealed trait Request

/**
  * Represents an evaluation request.
  *
  * @param expression expression to evaluate
  */
case class EvaluationRequest(expression: String) extends Request

/**
  * Represents a list bindings request.
  */
object ListBindingsRequest extends Request

/**
  * Represents a request to end debugger session.
  */
object SessionExitRequest extends Request
