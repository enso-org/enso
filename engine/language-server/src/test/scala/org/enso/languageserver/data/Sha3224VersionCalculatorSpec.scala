package org.enso.languageserver.data

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Sha3224VersionCalculatorSpec extends AnyFlatSpec with Matchers {

  "A Sha3Digest" should "produce SHA3-224 digest" in {
    Sha3_224VersionCalculator.evalVersion(
      " "
    ) mustBe "4cb5f87b01b38adc0e6f13f915668c2394cb1fb7a2795635b894dda1"
    Sha3_224VersionCalculator.evalVersion(
      "The quick brown fox jumps over the lazy dog"
    ) mustBe "d15dadceaa4d5d7bb3b48f446421d542e08ad8887305e28d58335795"
  }

}
