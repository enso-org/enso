package org.enso.interpreter.test.instrument

import org.enso.polyglot.{LanguageInfo, RuntimeOptions}
import org.graalvm.polyglot.{Context, PolyglotException}
import org.scalatest.{BeforeAndAfterEach, Suite}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths
import java.util.logging.Level

trait WithContext extends BeforeAndAfterEach { this: Suite =>
  var context: Context = null

  override def afterEach(): Unit = {
    if (context != null) {
      context.close()
    }
    super.afterEach()
  }

}

class RuntimeProjectContextTest
    extends AnyWordSpec
    with Matchers
    with WithContext {
  "Runtime Context" should {
    "report an exception if ran in context of a project " +
    "which cannot be loaded" in {
      val thrown = intercept[PolyglotException] {
        context = Context
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
          .option("engine.WarnInterpreterOnly", "false")
          .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
          .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName)
          .logHandler(System.err)
          .build()
        context.initialize(LanguageInfo.ID)
      }
      thrown.getMessage should include("ProjectLoadingFailure")
    }
  }
}
