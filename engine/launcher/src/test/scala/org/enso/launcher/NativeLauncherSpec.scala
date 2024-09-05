package org.enso.launcher

import io.circe.parser
import org.enso.version.BuildVersion

class NativeLauncherSpec extends NativeTest {
  "native launcher" should {
    "display its version" in {
      val run = runLauncher(
        Seq("version", "--json", "--only-launcher"),
        extraJVMProps  = Map("ENSO_LOG_TO_FILE" -> "false"),
        timeoutSeconds = 30
      )
      run should returnSuccess
      val out = run.stdout

      val version = parser.parse(out).getOrElse {
        throw new RuntimeException(
          "Version should be a valid JSON string. Got: " + out
        )
      }
      version.asObject.get
        .apply("version")
        .get
        .asString
        .get shouldEqual BuildVersion.ensoVersion
    }
  }
}
