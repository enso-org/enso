package org.enso.runtimeversionmanager.releases.engine

import java.nio.file.Path

import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.runtimeversionmanager.releases.github.GithubReleaseProvider
import org.enso.runtimeversionmanager.releases.local.LocalReleaseProvider

/** Represents the default Enso repository providing releases of the engine. */
object EngineRepository {
  val githubRepository = new GithubReleaseProvider("enso-org", "enso")

  /** Default provider of engine releases. */
  def defaultEngineReleaseProvider: ReleaseProvider[EngineRelease] =
    new EngineReleaseProvider(githubRepository)

  /** Creates an engine provider that uses a local repository first, falling
    * back to the default one.
    */
  def fromLocalRepository(
    releaseDirectory: Path
  ): ReleaseProvider[EngineRelease] = {
    val mergedRepository =
      new LocalReleaseProvider(releaseDirectory, githubRepository)
    new EngineReleaseProvider(mergedRepository)
  }
}
