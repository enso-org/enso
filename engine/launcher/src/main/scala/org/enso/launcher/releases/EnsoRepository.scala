package org.enso.launcher.releases

import java.nio.file.Path

import com.typesafe.scalalogging.Logger
import org.enso.runtimeversionmanager.http.URIBuilder
import org.enso.runtimeversionmanager.releases.engine.{
  EngineRelease,
  EngineReleaseProvider
}
import org.enso.runtimeversionmanager.releases.github.GithubReleaseProvider
import org.enso.runtimeversionmanager.releases.testing.FakeReleaseProvider
import org.enso.runtimeversionmanager.releases.{
  ReleaseProvider,
  SimpleReleaseProvider
}
import org.enso.launcher.distribution.DefaultManagers
import org.enso.launcher.releases.fallback.SimpleReleaseProviderWithFallback
import org.enso.launcher.releases.fallback.staticwebsite.StaticWebsiteFallbackReleaseProvider
import org.enso.launcher.releases.launcher.{
  LauncherRelease,
  LauncherReleaseProvider
}

/** Represents the default Enso repository providing releases for the engine and
  * the launcher.
  *
  * In test mode, the default GitHub repository can be overridden with a local
  * filesystem-backed repository.
  */
object EnsoRepository {
  // TODO [RW] The release provider will be moved from staging to the main
  //  repository, when the first official Enso release is released.
  private val githubRepository = new GithubReleaseProvider(
    "enso-org",
    "enso-staging"
  )

  /** Defines the URL of the fallback mechanism.
    *
    * That URL must *never* be changed to ensure that all older launcher
    * versions can be upgraded.
    */
  private val launcherFallbackProviderHostname =
    "launcherfallback.release.enso.org"

  /** Defines a part of the URL scheme of the fallback mechanism - the name of
    * the directory that holds the releases.
    *
    * That must *never* be changed to ensure that all older launcher versions
    * can be upgraded.
    */
  private val launcherFallbackReleaseDirectory = "launcher"

  private val launcherS3Fallback = new StaticWebsiteFallbackReleaseProvider(
    URIBuilder.fromHost(launcherFallbackProviderHostname),
    launcherFallbackReleaseDirectory
  )

  /** Default provider of engine releases.
    */
  def defaultEngineReleaseProvider: ReleaseProvider[EngineRelease] =
    new EngineReleaseProvider(defaultEngineRepository)

  /** Default provider of launcher releases.
    */
  def defaultLauncherReleaseProvider: ReleaseProvider[LauncherRelease] =
    new LauncherReleaseProvider(launcherRepository)

  /** Overrides the default repository with a local filesystem based fake
    * repository.
    *
    * Currently only the launcher repository is overridden.
    *
    * Internal method used for testing.
    */
  def internalUseFakeRepository(
    fakeRepositoryRoot: Path,
    shouldWaitForAssets: Boolean
  ): Unit =
    if (buildinfo.Info.isRelease)
      throw new IllegalStateException(
        "Internal testing function internalUseFakeRepository used in a " +
        "release build."
      )
    else {
      Logger("TEST").debug(s"Using a fake repository at $fakeRepositoryRoot.")
      launcherRepository =
        makeFakeRepository(fakeRepositoryRoot, shouldWaitForAssets)
    }

  private val defaultEngineRepository = githubRepository
  private val defaultLauncherRepository = new SimpleReleaseProviderWithFallback(
    baseProvider     = githubRepository,
    fallbackProvider = launcherS3Fallback
  )
  private var launcherRepository: SimpleReleaseProvider =
    defaultLauncherRepository

  private def makeFakeRepository(
    fakeRepositoryRoot: Path,
    shouldWaitForAssets: Boolean
  ): SimpleReleaseProvider =
    FakeReleaseProvider(
      fakeRepositoryRoot,
      lockManagerForAssets =
        if (shouldWaitForAssets) Some(DefaultManagers.DefaultFileLockManager)
        else None
    )
}
