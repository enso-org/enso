package org.enso.launcher

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import buildinfo.Info

class NativeLauncherSpec extends AnyWordSpec with Matchers with NativeTest {
  "native launcher" should {
    "display its version" in {
      val run = runLauncher(Seq("--version"))
      run should returnSuccess

      run.stdout should include("Enso Launcher")
      run.stdout should include("Version:")
      run.stdout should include(Info.ensoVersion)
    }
  }
}
