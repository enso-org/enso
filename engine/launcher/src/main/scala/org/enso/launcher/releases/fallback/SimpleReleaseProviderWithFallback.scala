package org.enso.launcher.releases.fallback

import org.enso.launcher.releases.{Release, SimpleReleaseProvider}

import scala.util.{Failure, Try}

/** Extends a base release provider with a fallback method.
  *
  * If the `baseProvider` fails, the fallback method is checked for availability
  * and if it is available, it tries to recover. If the fallback method is
  * unavailable, the original error is reported.
  */
class SimpleReleaseProviderWithFallback(
  baseProvider: SimpleReleaseProvider,
  fallbackProvider: FallbackReleaseProvider
) extends SimpleReleaseProvider {

  /** @inheritdoc
    */
  override def releaseForTag(tag: String): Try[Release] =
    baseProvider.releaseForTag(tag).recoverWith { error =>
      if (fallbackProvider.isEnabled)
        fallbackProvider.releaseForTag(tag)
      else Failure(error)
    }

  /** @inheritdoc
    */
  override def listReleases(): Try[Seq[Release]] =
    baseProvider.listReleases().recoverWith { error =>
      if (fallbackProvider.isEnabled)
        fallbackProvider.listReleases()
      else Failure(error)
    }
}
