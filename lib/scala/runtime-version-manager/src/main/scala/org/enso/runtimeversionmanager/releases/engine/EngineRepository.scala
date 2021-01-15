package org.enso.runtimeversionmanager.releases.engine

import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.runtimeversionmanager.releases.github.GithubReleaseProvider

/** Represents the default Enso repository providing releases of the engine. */
object EngineRepository {
  val githubRepository = new GithubReleaseProvider("enso-org", "enso")

  /** Default provider of engine releases. */
  def defaultEngineReleaseProvider: ReleaseProvider[EngineRelease] =
    new EngineReleaseProvider(githubRepository)
}
