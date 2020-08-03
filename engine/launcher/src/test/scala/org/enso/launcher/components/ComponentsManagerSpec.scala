package org.enso.launcher.components

import org.enso.launcher.WithTemporaryDirectory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentsManagerSpec
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory {
  "ComponentsManager" should {
    "find the latest engine version in semver ordering" in {
      // TODO
    }

    "install the engine and a matching runtime for it" in {
      // TODO
    }

    "list installed engines and runtimes" in {
      // TODO
    }

    "uninstall the runtime iff it is not used by any engines" in {
      // TODO
    }
  }
}
