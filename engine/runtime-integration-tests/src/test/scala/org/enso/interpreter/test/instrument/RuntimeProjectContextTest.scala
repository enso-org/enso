package org.enso.interpreter.test.instrument

import org.enso.common.LanguageInfo
import org.enso.common.RuntimeOptions
import org.enso.logger.JulHandler
import org.enso.testkit.ReportLogsOnFailure
import org.graalvm.polyglot.{Context, PolyglotException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.BeforeAndAfterEach

import java.nio.file.Paths

class RuntimeProjectContextTest
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterEach
    with ReportLogsOnFailure {

  var context: Context = _

  override def afterEach(): Unit = {
    if (context != null) {
      context.close()
    }
    super.afterEach()
  }

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
          .option(RuntimeOptions.EDITION_OVERRIDE, "0.0.0-dev")
          .option(
            RuntimeOptions.LOG_LEVEL,
            java.util.logging.Level.WARNING.getName
          )
          .logHandler(JulHandler.get)
          .build()
        context.initialize(LanguageInfo.ID)
      }
      thrown.getMessage should include("ProjectLoadingFailure")
    }
  }
}
