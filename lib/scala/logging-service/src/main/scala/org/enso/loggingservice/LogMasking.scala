package org.enso.loggingservice

import org.enso.logger.ToMaskedString
import org.enso.loggingservice.internal.InternalLogger

import scala.util.control.NonFatal

trait LogMasking {

  /** Converts the provided object to a masked representation.
    *
    * @param obj the object to mask
    * @return the masked object
    */
  def mask(obj: AnyRef): AnyRef
}
object LogMasking {

  @volatile private var maskingEnabled: Boolean = true

  /** Environment variable name that disables log masking. */
  val NO_LOG_MASKING = "NO_LOG_MASKING"

  /** Setup the log masking.
    *
    * @param enabled if the log masking is enabled
    */
  def setup(enabled: Boolean): Unit = {
    maskingEnabled = enabled
  }

  /** Checks if the log masking is enabled. */
  def isMaskingEnabled: Boolean = {
    !sys.env.contains(NO_LOG_MASKING) &&
    maskingEnabled
  }

  private val masking: LogMasking = {
    case obj: ToMaskedString =>
      try obj.toMaskedString
      catch {
        case NonFatal(error) =>
          InternalLogger.error(
            "Failed `toMaskedString` invocation on object of type " +
            s"'${obj.getClass.getName}'. " +
            s"${error.getClass.getName}: " +
            s"${error.getMessage}"
          )
          "[Failed `toMaskedString`]"
      }
    case obj => obj
  }

  private val noMasking: LogMasking = identity

  /** Create the masking adapter. */
  def apply(): LogMasking =
    if (isMaskingEnabled) masking else noMasking
}
