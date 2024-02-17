package org.enso.launcher.components

import com.github.zafarkhaja.semver.Version
import org.enso.launcher.Constants
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UploadVersionCheck extends AnyWordSpec with Matchers {
  "Constants.uploadIntroducedVersion" should {
    "correctly compare with nearby versions" in {
      assert(
        Version
          .parse("0.2.17-SNAPSHOT.2021-07-23")
          .isHigherThanOrEquivalentTo(
            Constants.uploadIntroducedVersion
          )
      )

      assert(
        Version
          .parse("0.2.17")
          .isHigherThan(
            Constants.uploadIntroducedVersion
          )
      )

      assert(
        Version
          .parse("0.2.16")
          .isLowerThan(
            Constants.uploadIntroducedVersion
          )
      )
    }
  }
}
