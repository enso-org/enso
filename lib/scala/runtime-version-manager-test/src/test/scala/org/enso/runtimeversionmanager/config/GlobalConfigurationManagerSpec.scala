package org.enso.runtimeversionmanager.config

import io.circe.Json
import org.enso.semver.SemVer
import org.enso.distribution.DistributionManager
import org.enso.distribution.config.InvalidConfigError
import org.enso.runtimeversionmanager.test.FakeEnvironment
import org.enso.testkit.WithTemporaryDirectory
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GlobalConfigurationManagerSpec
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with FakeEnvironment
    with OptionValues {
  def makeConfigManager(): GlobalRunnerConfigurationManager = {
    val env                 = fakeInstalledEnvironment()
    val distributionManager = new DistributionManager(env)
    new GlobalRunnerConfigurationManager(null, distributionManager) {
      override def defaultVersion: SemVer = SemVer.of(0, 0, 0)
    }
  }

  "GlobalConfigurationManager" should {
    "allow to edit and remove (even unknown) keys" in {
      val configurationManager = makeConfigManager()
      val value                = Json.fromInt(42)
      configurationManager.updateConfigRaw("unknown-key", value)
      configurationManager.getConfig.original
        .apply("unknown-key")
        .value shouldEqual value
      configurationManager.removeFromConfig("unknown-key")
      configurationManager.getConfig.original.apply(
        "unknown-key"
      ) should not be defined
    }

    "not allow saving an invalid config" in {
      val configurationManager = makeConfigManager()
      intercept[InvalidConfigError] {
        configurationManager.updateConfigRaw(
          "default.enso-version",
          Json.fromString("invalid-version")
        )
      }
    }
  }

}
