package org.enso.launcher.releases.fallback

import org.enso.launcher.releases.{Asset, Release, SimpleReleaseProvider}
import org.scalatest.TryValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success, Try}

class SimpleReleaseProviderWithFallbackSpec
    extends AnyWordSpec
    with Matchers
    with TryValues {
  case object BaseProviderException     extends RuntimeException
  case object FallbackProviderException extends RuntimeException

  case class TestRelease(tag: String) extends Release {
    override def assets: Seq[Asset] =
      throw new NotImplementedError("Not implemented for testing purposes.")
  }

  class BaseProvider(broken: Boolean, releases: Seq[TestRelease])
      extends SimpleReleaseProvider {
    override def releaseForTag(tag: String): Try[Release] =
      listReleases().flatMap { releases =>
        releases
          .find(_.tag == tag)
          .map(Success(_))
          .getOrElse(Failure(BaseProviderException))
      }
    override def listReleases(): Try[Seq[Release]] =
      if (broken) Failure(BaseProviderException)
      else Success(releases)
  }

  class FallbackProvider(
    enabled: Boolean,
    broken: Boolean,
    releases: Seq[TestRelease]
  ) extends FallbackReleaseProvider {
    override def isEnabled: Boolean = enabled
    override def releaseForTag(tag: String): Try[Release] =
      listReleases().flatMap { releases =>
        releases
          .find(_.tag == tag)
          .map(Success(_))
          .getOrElse(Failure(FallbackProviderException))
      }

    override def listReleases(): Try[Seq[Release]] =
      if (broken || !isEnabled) Failure(FallbackProviderException)
      else Success(releases)
  }

  def makeProvider(
    baseBroken: Boolean,
    fallbackAvailable: Boolean,
    fallbackBroken: Boolean
  ): SimpleReleaseProviderWithFallback = {
    val releases = Seq(TestRelease("v1"))
    val base     = new BaseProvider(broken = baseBroken, releases = releases)
    val fallback = new FallbackProvider(
      enabled  = fallbackAvailable,
      broken   = fallbackBroken,
      releases = releases
    )
    new SimpleReleaseProviderWithFallback(base, fallback)
  }

  "SimpleReleaseProviderWithFallback" should {
    "use the base provider in normal situations" in {
      val provider = makeProvider(
        baseBroken        = false,
        fallbackAvailable = true,
        fallbackBroken    = true
      )

      provider.listReleases().success.value
      provider.releaseForTag("v1").success.value
    }

    "use fallback provider if base fails and it is available" in {
      val provider = makeProvider(
        baseBroken        = true,
        fallbackAvailable = true,
        fallbackBroken    = false
      )

      provider.listReleases().success.value
      provider.releaseForTag("v1").success.value
      provider.releaseForTag("404") shouldEqual Failure(
        FallbackProviderException
      )

      val broken = makeProvider(
        baseBroken        = true,
        fallbackAvailable = true,
        fallbackBroken    = true
      )
      broken.listReleases() shouldEqual Failure(FallbackProviderException)
    }

    "do not use an unavailable fallback provider" in {
      val provider = makeProvider(
        baseBroken        = false,
        fallbackAvailable = false,
        fallbackBroken    = true
      )

      provider.listReleases().success.value
      provider.releaseForTag("v1").success.value
      provider.releaseForTag("404") shouldEqual Failure(BaseProviderException)

      val broken = makeProvider(
        baseBroken        = true,
        fallbackAvailable = false,
        fallbackBroken    = true
      )
      broken.listReleases() shouldEqual Failure(BaseProviderException)
      broken.releaseForTag("v1") shouldEqual Failure(BaseProviderException)
    }
  }
}
