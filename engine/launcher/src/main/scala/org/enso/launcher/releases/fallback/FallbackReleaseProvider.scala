package org.enso.launcher.releases.fallback

import org.enso.launcher.releases.SimpleReleaseProvider

trait FallbackReleaseProvider extends SimpleReleaseProvider {
  def isAvailable: Boolean
}
