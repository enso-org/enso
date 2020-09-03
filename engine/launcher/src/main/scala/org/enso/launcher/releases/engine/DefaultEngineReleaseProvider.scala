package org.enso.launcher.releases.engine

import org.enso.launcher.releases.EnsoRepository

/**
  * Default [[EngineReleaseProvider]] that uses the GitHub Release API.
  */
object DefaultEngineReleaseProvider
    extends EngineReleaseProvider(EnsoRepository)
