package org.enso.launcher.releases.fallback

import org.enso.launcher.releases.SimpleReleaseProvider

/**
  * A fallback [[SimpleReleaseProvider]].
  *
  * It should implement all the functionality of the [[SimpleReleaseProvider]],
  * but it is extended with a notion of availability. This provider should only
  * be used if it is enabled.
  */
trait FallbackReleaseProvider extends SimpleReleaseProvider {

  /**
    * Specifies if the fallback release provider is enabled and can be used.
    */
  def isEnabled: Boolean
}
