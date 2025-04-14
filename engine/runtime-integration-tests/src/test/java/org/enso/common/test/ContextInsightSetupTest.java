package org.enso.common.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileWriter;
import org.enso.test.utils.ContextRule;
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
    try (var ctx = ContextRule.createDefault()) {

      var fourtyTwo = ctx.evalModule("""
        main = 42
        """);

      assertEquals("42", fourtyTwo.toString());

      assertThat(
          out.toString(),
          AllOf.allOf(
              containsString("Insight started."), containsString("Properties: id,version")));
    }
  }
}
