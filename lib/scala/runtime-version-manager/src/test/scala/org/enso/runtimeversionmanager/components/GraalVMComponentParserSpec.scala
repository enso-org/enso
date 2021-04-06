package org.enso.runtimeversionmanager.components

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GraalVMComponentParserSpec extends AnyWordSpec with Matchers {

  "RuntimeComponentUpdater" should {

    "parse list output" in {
      val listOut =
        """
          |ID       : js
          |Name     : Graal.js
          |Version  : 21.0.0.2
          |GraalVM  : n/a
          |Stability: -
          |Origin   :
          |
          |ID       : graalvm
          |Name     : GraalVM Core
          |Version  : 21.0.0.2
          |GraalVM  : n/a
          |Stability: -
          |Origin   :
          |
          |ID       : R
          |Name     : FastR
          |Version  : 21.0.0.2
          |GraalVM  : 21.0.0.2
          |Stability: Experimental
          |Origin   : https://github.com/oracle/fastr/releases/download/vm-21.0.0.2/r-installable-java11-linux-amd64-21.0.0.2.jar
          |
          |ID       : llvm-toolchain
          |Name     : LLVM.org toolchain
          |Version  : 21.0.0.2
          |GraalVM  : 21.0.0.2
          |Stability: Supported
          |Origin   : https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.0.0.2/llvm-toolchain-installable-java11-linux-amd64-21.0.0.2.jar
          |
          |ID       : native-image
          |Name     : Native Image
          |Version  : 21.0.0.2
          |GraalVM  : 21.0.0.2
          |Stability: Early adopter
          |Origin   : https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-21.0.0.2/native-image-installable-svm-java11-linux-amd64-21.0.0.2.jar
          |""".stripMargin

      val expectedComponents =
        Seq("js", "graalvm", "R", "llvm-toolchain", "native-image")
          .map(GraalVMComponent(_))
      val components =
        GraalVMComponentUpdater.ListOut.parse(listOut.linesIterator.toSeq)
      components shouldEqual expectedComponents
    }
  }
}
