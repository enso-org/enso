package org.enso.languageserver.boot

import scala.concurrent.duration._

/** TimingsConfig encapsulates information about timings or delays in messages being sent between services.
  *
  * @param timeout a request timeout
  * @param autoSave if non-empty value, determines the delay when auto-save should be triggered
  */
class TimingsConfig(
  private[this] val timeout: FiniteDuration,
  private[this] var autoSave: Option[FiniteDuration]
) {
  def this(timeout: FiniteDuration) = {
    this(timeout, None)
  }

  /** A request timeout.
    *
    * @return a duration to wait for the request to be handled
    */
  def requestTimeout: FiniteDuration = timeout

  /** A (internal) request timeout.
    * It should be smaller than the `requestTimeout` to properly propagate failure upwards rather
    * than request handler timing out.
    *
    * @return a duration to wait for the request to runtime to be handled
    */
  def runtimeRequestTimeout: FiniteDuration = timeout * 3 / 4

  /** Auto-save delay.
    *
    * @return if non-empty, determines the delay when auto-save should be triggered after the last edit
    */
  def autoSaveDelay: Option[FiniteDuration] = autoSave

  /** Sets the delay for auto-save action that should be triggered after the last edit action.
    *
    * @param delay delay for auto-save action
    * @return updated config
    */
  def withAutoSave(delay: FiniteDuration): TimingsConfig = {
    autoSave = Some(delay)
    this
  }
}

object TimingsConfig {
  def default(): TimingsConfig = new TimingsConfig(10.seconds)
}
