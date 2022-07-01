package org.enso.interpreter.test.instrument

import org.enso.polyglot.{LanguageInfo, RuntimeOptions}
import org.graalvm.polyglot.{Context, PolyglotException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class RuntimeProjectContextTest extends AnyWordSpec with Matchers {
  "Runtime Context" should {
    "report an exception if ran in context of a project " +
    "which cannot be loaded" in {
      val thrown = intercept[PolyglotException] {
        val context = Context
          .newBuilder(LanguageInfo.ID)
          .allowExperimentalOptions(true)
          .allowAllAccess(true)
          .option(
            RuntimeOptions.PROJECT_ROOT,
            Paths.get("../../target/fakeproject_dir").toFile.getAbsolutePath
          )
          .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths
              .get("../../test/micro-distribution/component")
              .toFile
              .getAbsolutePath
          )
          .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
          .option(RuntimeOptions.LOG_LEVEL, "WARNING")
          .build()
        context.initialize(LanguageInfo.ID)
      }
      thrown.getMessage should include("ProjectLoadingFailure")
    }
  }
}
