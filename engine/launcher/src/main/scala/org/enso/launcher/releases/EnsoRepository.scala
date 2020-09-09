package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.launcher.Logger
import org.enso.launcher.releases.engine.{EngineRelease, EngineReleaseProvider}
import org.enso.launcher.releases.github.GithubReleaseProvider
import org.enso.launcher.releases.launcher.{
  LauncherRelease,
  LauncherReleaseProvider
}
import org.enso.launcher.releases.testing.FakeReleaseProvider

/**
  * Represents the default Enso repository providing releases for the engine and
  * the launcher.
  *
  * In test mode, the default GitHub repository can be overridden with a local
  * filesystem-backed repository.
  */
object EnsoRepository {
  private var currentRepository: SimpleReleaseProvider =
    // TODO [RW] The release provider will be moved from staging to the main
    //  repository, when the first official Enso release is released.
    new GithubReleaseProvider(
      "enso-org",
      "enso-staging"
    )

  /**
    * Default repository for Enso releases.
    */
  def defaultReleaseRepository: SimpleReleaseProvider = currentRepository

  /**
    * Default provider of engine releases.
    */
  def defaultEngineReleaseProvider: ReleaseProvider[EngineRelease] =
    new EngineReleaseProvider(defaultReleaseRepository)

  /**
    * Default provider of launcher releases.
    */
  def defaultLauncherReleaseProvider: ReleaseProvider[LauncherRelease] =
    new LauncherReleaseProvider(defaultReleaseRepository)

  /**
    * Overrides the default repository with a local filesystem based fake
    * repository.
    *
    * Internal method used for testing.
    */
  def internalUseFakeRepository(fakeRepositoryRoot: Path): Unit =
    if (buildinfo.Info.isRelease)
      throw new IllegalStateException(
        "Internal testing function internalUseFakeRepository used in a " +
        "release build."
      )
    else {
      Logger.debug(s"[TEST] Using a fake repository at $fakeRepositoryRoot.")
      currentRepository = makeFakeRepository(fakeRepositoryRoot)
    }

  private def makeFakeRepository(
    fakeRepositoryRoot: Path
  ): SimpleReleaseProvider =
    FakeReleaseProvider(fakeRepositoryRoot)
}
