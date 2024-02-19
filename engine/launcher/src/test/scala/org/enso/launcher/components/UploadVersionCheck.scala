package org.enso.launcher.components

import org.enso.semver.SemVer
import org.enso.launcher.Constants
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UploadVersionCheck extends AnyWordSpec with Matchers {
  "Constants.uploadIntroducedVersion" should {
    "correctly compare with nearby versions" in {
      assert(
        SemVer
          .parse("0.2.17-SNAPSHOT.2021-07-23")
          .get
          .isGreaterThanOrEqual(
            Constants.uploadIntroducedVersion
          )
      )

      assert(
        SemVer
          .parse("0.2.17")
          .get
          .isGreaterThan(
            Constants.uploadIntroducedVersion
          )
      )

      assert(
        SemVer
          .parse("0.2.16")
          .get
          .isLessThan(
            Constants.uploadIntroducedVersion
          )
      )
    }
  }
}
