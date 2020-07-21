package org.enso.launcher

import buildinfo.Info

class NativeLauncherSpec extends NativeTest {
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
