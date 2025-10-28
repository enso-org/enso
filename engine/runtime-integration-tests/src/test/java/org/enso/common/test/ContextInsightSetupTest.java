package org.enso.common.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import org.enso.common.ContextFactory;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.hamcrest.core.AllOf;
import org.junit.AfterClass;
import org.junit.Test;

/**
 * Demonstrates usage of {@code -Denso.dev.insight=insightScript.js} property. This is a
 * developement only support for playing with GraalVM Insight scripts inside of the IDE as well in
 * CLI.
 */
public class ContextInsightSetupTest {

  public ContextInsightSetupTest() {}

  @AfterClass
  public static void cleanupInsightProperty() {
    System.getProperties().remove("enso.dev.insight");
  }

  @Test
  public void initializeInsightViaProperty() throws Exception {
    var insight = File.createTempFile("insight", ".js");
    try (java.io.FileWriter w = new FileWriter(insight)) {
      w.write(
          """
          print("Insight started. Properties: " + Object.getOwnPropertyNames(insight).sort());
          """);
    }

    System.setProperty("enso.dev.insight", insight.getPath());

    var out = new ByteArrayOutputStream();
    // Need to initialize the Context via ContextFactory, so that ContextInsightSetup is
    // triggered.
    try (var ctx = ContextFactory.create().out(out).build()) {

      var fourtyTwo =
          evalModule(
              ctx,
              """
              main = 42
              """);

      assertEquals("42", fourtyTwo.toString());

      assertThat(
          out.toString(),
          AllOf.allOf(
              containsString("Insight started."), containsString("Properties: id,version")));
    }
  }

  private static Value evalModule(Context ctx, String moduleSrc) {
    var module = ctx.eval(LanguageInfo.ID, moduleSrc);
    var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var method = module.invokeMember(Module.GET_METHOD, assocType, "main");
    return method.execute();
  }
}
