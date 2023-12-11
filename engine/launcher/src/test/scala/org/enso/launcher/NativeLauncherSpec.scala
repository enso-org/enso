package org.enso.launcher

import buildinfo.Info
import io.circe.parser

class NativeLauncherSpec extends NativeTest {
  "native launcher" should {
    "display its version" in {
      val run = runLauncher(
        Seq("version", "--json", "--only-launcher"),
        extraJVMProps  = Map("ENSO_LOG_TO_FILE" -> "false"),
        timeoutSeconds = 30
      )
      run should returnSuccess

      val version = parser.parse(run.stdout).getOrElse {
        throw new RuntimeException("Version should be a valid JSON string.")
      }
      version.asObject.get
        .apply("version")
        .get
        .asString
        .get shouldEqual Info.ensoVersion
    }
  }
}
