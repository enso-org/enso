package org.enso.languageserver.runtime

import io.circe.{Decoder, Encoder}
import org.enso.polyglot.runtime.ExecutionEnvironment

/** Base trait for the execution environment. */
object ExecutionEnvironments extends Enumeration {
  type Env = Value

  val Design, Live = Value

  /** Create an execution environment from the polyglot environment.
    *
    * @param executionEnvironment the polyglot execution environment
    * @return corresponding execution environment object
    */
  def apply(
    executionEnvironment: ExecutionEnvironment
  ): Env =
    executionEnvironment match {
      case _: ExecutionEnvironment.Design => Design
      case _: ExecutionEnvironment.Live   => Live
    }

  /** Convert the execution environment to the appropriate API type.
    *
    * @param executionEnvironment the execution environment
    * @return corresponding Api object
    */
  def toApi(
    executionEnvironment: Env
  ): ExecutionEnvironment =
    executionEnvironment match {
      case Design => ExecutionEnvironment.Design()
      case Live   => ExecutionEnvironment.Live()
    }

  implicit val genderDecoder: Decoder[Env] =
    Decoder.decodeEnumeration(ExecutionEnvironments)
  implicit val genderEncoder: Encoder[Env] =
    Encoder.encodeEnumeration(ExecutionEnvironments)
}
