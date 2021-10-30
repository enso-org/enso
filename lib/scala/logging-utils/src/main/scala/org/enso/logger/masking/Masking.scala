package org.enso.logger.masking

import scala.util.control.NonFatal

trait Masking {

  /** Converts the provided object to a masked representation.
    *
    * @param obj the object to mask
    * @return the masked object
    */
  def mask(obj: AnyRef): AnyRef
}
object Masking {

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

  private def masking(shouldMask: Boolean): Masking = {
    case obj: ToLogString =>
      try obj.toLogString(shouldMask)
      catch {
        case NonFatal(error) =>
          System.err.println(
            "[internal-logger-error] " +
            "Failed `toMaskedString` invocation on object of type " +
            s"'${obj.getClass.getName}'. " +
            s"${error.getClass.getName}: " +
            s"${error.getMessage}"
          )
          "[Failed `toMaskedString`]"
      }
    case obj => obj
  }

  /** Get the instance of [[Masking]] adapter. */
  def apply(): Masking = masking(isMaskingEnabled)

}
