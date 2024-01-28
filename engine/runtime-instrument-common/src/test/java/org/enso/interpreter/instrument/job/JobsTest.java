package org.enso.interpreter.instrument.job;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.UUID;
import org.enso.polyglot.runtime.Runtime$Api$VisualizationConfiguration;
import org.enso.polyglot.runtime.Runtime$Api$VisualizationExpression$Text;
import org.junit.Test;
import scala.Option;

public class JobsTest {
  @Test
  public void upsertJobUniqueness() {
    var config =
        new Runtime$Api$VisualizationConfiguration(
            UUID.randomUUID(), new Runtime$Api$VisualizationExpression$Text("foo", "bar"), "test");
    var expression1 = UUID.randomUUID();
    var expression2 = UUID.randomUUID();
    var visualization1 = UUID.randomUUID();
    var visualization2 = UUID.randomUUID();
    var visualization3 = UUID.randomUUID();

    var upsert1 = new UpsertVisualizationJob(Option.empty(), visualization1, expression1, config);
    var upsert2 = new UpsertVisualizationJob(Option.empty(), visualization2, expression1, config);
    var upsert3 = new UpsertVisualizationJob(Option.empty(), visualization3, expression2, config);
    var upsert4 = new UpsertVisualizationJob(Option.empty(), visualization1, expression1, config);
    var upsert5 = new UpsertVisualizationJob(Option.empty(), visualization1, expression2, config);

    assertFalse(upsert1.equalsTo(upsert2));
    assertFalse(upsert2.equalsTo(upsert3));
    assertFalse(upsert1.equalsTo(upsert3));
    assertTrue(upsert1.equalsTo(upsert4));
    assertFalse(upsert3.equalsTo(upsert4));
    assertFalse(upsert5.equalsTo(upsert3));
  }
}
