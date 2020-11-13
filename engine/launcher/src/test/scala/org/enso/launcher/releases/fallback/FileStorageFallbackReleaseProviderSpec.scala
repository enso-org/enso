package org.enso.launcher.releases.fallback

import java.nio.file.Path

import org.enso.runtimeversionmanager.FileSystem
import org.enso.launcher.TestHelpers
import org.enso.launcher.releases.fallback.staticwebsite.FileStorageFallbackReleaseProvider
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{Inside, OptionValues}

import scala.util.Failure

class FileStorageFallbackReleaseProviderSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with Inside {
  private val fakeStorageRoot =
    Path.of(
      getClass
        .getResource("/org/enso/launcher/releases/fallback")
        .toURI
    )

  sealed trait Provider
  object Provider {
    case object Available   extends Provider
    case object Unavailable extends Provider
  }

  private def makeProvider(
    provider: Provider
  ): FallbackReleaseProvider = {
    val backingDirectory = provider match {
      case Provider.Available   => "available"
      case Provider.Unavailable => "unavailable"
    }
    val storage = new TestFileStorage(fakeStorageRoot.resolve(backingDirectory))
    new FileStorageFallbackReleaseProvider(storage, "launcher")
  }

  "FileStorageFallbackReleaseProvider" should {
    "correctly report available" in {
      makeProvider(Provider.Available).isEnabled shouldEqual true
      makeProvider(Provider.Unavailable).isEnabled shouldEqual false
    }

    "fail with IllegalStateException if it is queried but unavailable" in {
      val provider = makeProvider(Provider.Unavailable)
      inside(provider.listReleases()) { case Failure(exception) =>
        exception shouldBe an[IllegalStateException]
        exception.getMessage should include("provider is unavailable")
      }

      inside(provider.releaseForTag("tag")) { case Failure(exception) =>
        exception shouldBe an[IllegalStateException]
        exception.getMessage should include("provider is unavailable")
      }
    }

    "list releases" in {
      val provider = makeProvider(Provider.Available)
      val releases = provider.listReleases().get
      releases(0).tag shouldEqual "enso-0.0.0"
      releases(0).assets.map(_.fileName).toSet shouldEqual Set(
        "data.txt",
        "broken"
      )
      releases(1).tag shouldEqual "enso-0.0.1"
      releases(1).assets.map(_.fileName).toSet shouldEqual Set("data.txt")
    }

    "fetch text from given release" in {
      val provider = makeProvider(Provider.Available)
      val assets   = provider.releaseForTag("enso-0.0.0").get.assets
      val asset    = assets.find(_.fileName == "data.txt").value
      asset.fetchAsText().waitForResult().get.trim shouldEqual "Older Broken"
    }

    "download files from given release" in {
      val provider = makeProvider(Provider.Available)
      val assets   = provider.releaseForTag("enso-0.0.1").get.assets
      val asset    = assets.find(_.fileName == "data.txt").value

      FileSystem.withTemporaryDirectory("enso-test") { temporaryDirectory =>
        val destination = temporaryDirectory.resolve("file.txt")
        asset.downloadTo(destination).waitForResult().get
        TestHelpers.readFileContent(destination).trim shouldEqual "Newer"
      }

    }
  }
}
