package org.enso.languageserver.runtime

import enumeratum._
import org.enso.polyglot.runtime.Runtime.Api

/** Base trait for the execution environment. */
sealed trait ExecutionEnvironment extends EnumEntry

object ExecutionEnvironment
    extends Enum[ExecutionEnvironment]
    with CirceEnum[ExecutionEnvironment] {

  case object Design extends ExecutionEnvironment
  case object Live   extends ExecutionEnvironment

  override val values: IndexedSeq[ExecutionEnvironment] =
    findValues

  /** Create an execution environment from the polyglot environment.
    *
    * @param executionEnvironment the polyglot execution environment
    * @return corresponding execution environment object
    */
  def apply(
    executionEnvironment: Api.ExecutionEnvironment
  ): ExecutionEnvironment =
    executionEnvironment match {
      case _: Api.ExecutionEnvironment.Design => Design
      case _: Api.ExecutionEnvironment.Live   => Live
    }

  /** Convert the execution environment to the appropriate API type.
    *
    * @param executionEnvironment the execution environment
    * @return corresponding Api object
    */
  def toApi(
    executionEnvironment: ExecutionEnvironment
  ): Api.ExecutionEnvironment =
    executionEnvironment match {
      case Design => Api.ExecutionEnvironment.Design()
      case Live   => Api.ExecutionEnvironment.Live()
    }
}
