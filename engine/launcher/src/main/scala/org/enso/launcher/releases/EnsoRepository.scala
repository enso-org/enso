package org.enso.launcher.releases

import org.enso.launcher.releases.github.GithubReleaseProvider

object EnsoRepository
    extends GithubReleaseProvider(
      "enso-org",
      "enso-staging" // TODO [RW] The release provider will be moved from
      // staging to the main repository, when the first official Enso release
      // is released.
    )
