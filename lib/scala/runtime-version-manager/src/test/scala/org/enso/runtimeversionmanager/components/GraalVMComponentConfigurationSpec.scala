package org.enso.runtimeversionmanager.components

import org.enso.cli.OS
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraalVMComponentConfigurationSpec extends AnyWordSpec with Matchers {

  "RuntimeComponentConfiguration" should {

    "return required components" in {
      val conf            = new GraalVMComponentConfiguration
      val required        = Seq(GraalVMComponent.python, GraalVMComponent.R)
      val requiredAbove22 = required ++ Seq(GraalVMComponent.js)

      conf.getRequiredComponents(
        GraalVMVersion("21.0.0.2", "11"),
        OS.Linux
      ) should contain theSameElementsAs required

      conf.getRequiredComponents(
        GraalVMVersion("21.0.0.2", "11"),
        OS.MacOS
      ) should contain theSameElementsAs required

      conf.getRequiredComponents(
        GraalVMVersion("21.0.0.2", "11"),
        OS.Windows
      ) should contain theSameElementsAs Seq()

      conf.getRequiredComponents(
        GraalVMVersion("22.0.0.0", "11"),
        OS.Linux
      ) should contain theSameElementsAs requiredAbove22

      conf.getRequiredComponents(
        GraalVMVersion("22.3.1", "17"),
        OS.MacOS
      ) should contain theSameElementsAs requiredAbove22

      conf.getRequiredComponents(
        GraalVMVersion("22.3.1", "17"),
        OS.Windows
      ) should contain theSameElementsAs Seq(
        GraalVMComponent.js
      )

      conf.getRequiredComponents(
        GraalVMVersion("22.3.1", "17"),
        OS.Linux
      ) should contain theSameElementsAs Seq(
        GraalVMComponent.js,
        GraalVMComponent.python,
        GraalVMComponent.R
      )

      conf.getRequiredComponents(
        GraalVMVersion("20.0.0.0", "11"),
        OS.Linux
      ) should contain theSameElementsAs Seq()

      conf.getRequiredComponents(
        GraalVMVersion("23.0.0", "17.0.7+7.1"),
        OS.Linux
      ) should contain theSameElementsAs Seq(
        GraalVMComponent.js,
        GraalVMComponent.python
      )

      conf.getRequiredComponents(
        GraalVMVersion("23.0.0", "11"),
        OS.Linux
      ) should contain theSameElementsAs Seq(
        GraalVMComponent.js,
        GraalVMComponent.python
      )

      conf.getRequiredComponents(
        GraalVMVersion("23.0.0", "17.0.7+7.1"),
        OS.Windows
      ) should contain theSameElementsAs Seq(
        GraalVMComponent.js
      )
    }

    "return no required components for Truffle unchained" in {
      val conf = new GraalVMComponentConfiguration
      val versions = Seq(
        GraalVMVersion("23.0.0", "21"),
        GraalVMVersion("23.0.1", "21"),
        GraalVMVersion("23.0.0", "21.0.1"),
        GraalVMVersion("23.0.1", "21.0.1"),
        GraalVMVersion("23.0.0", "21.0.1+5.1"),
        GraalVMVersion("23.0.1", "21.0.1+5.1"),
        GraalVMVersion("23.1.0", "21")
      )
      versions.forall(_.isUnchained) shouldBe true
      versions.foreach(version => {
        conf.getRequiredComponents(
          version,
          OS.Linux
        ) shouldBe empty
      })
    }
  }
}
