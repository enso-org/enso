package org.enso.runtimeversionmanager.releases.engine

import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.runtimeversionmanager.releases.github.GithubReleaseProvider

/** Represents the default Enso repository providing releases of the engine. */
object EngineRepository {
  // TODO [RW] The release provider will be moved from staging to the main
  //  repository, when the first official Enso release is released.
  val githubRepository = new GithubReleaseProvider(
    "enso-org",
    "enso-staging"
  )

  private val defaultEngineRepository = EngineRepository.githubRepository

  /** Default provider of engine releases. */
  def defaultEngineReleaseProvider: ReleaseProvider[EngineRelease] =
    new EngineReleaseProvider(defaultEngineRepository)
}
