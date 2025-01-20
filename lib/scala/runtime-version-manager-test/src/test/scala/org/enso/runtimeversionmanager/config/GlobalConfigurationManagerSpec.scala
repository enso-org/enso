package org.enso.runtimeversionmanager.config

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
    "allow to edit and remove known keys" in {
      val configurationManager = makeConfigManager()
      val value                = 42.toString
      configurationManager.updateConfigRaw("unknown-key", value)
      configurationManager.getConfig
        .findByKey("unknown-key") should not be defined
      val newEmail = "foo@bar.com"
      configurationManager.getConfig
        .findByKey("author.email") should not be defined
      configurationManager.updateConfigRaw("author.email", newEmail)
      configurationManager.getConfig
        .findByKey("author.email") shouldEqual Some(newEmail)
    }

    "not allow saving an invalid config" in {
      val configurationManager = makeConfigManager()
      intercept[InvalidConfigError] {
        configurationManager.updateConfigRaw(
          "default.enso-version",
          "invalid-version"
        )
      }
    }
  }

}
