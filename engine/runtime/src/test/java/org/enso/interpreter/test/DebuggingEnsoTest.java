package org.enso.interpreter.test;

import com.oracle.truffle.api.debug.DebugException;
import com.oracle.truffle.api.debug.DebugValue;
import com.oracle.truffle.api.debug.Debugger;
import com.oracle.truffle.api.debug.SuspendedEvent;
import java.net.URI;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.junit.Assert;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class DebuggingEnsoTest {
  @Test
  public void evaluation() throws Exception {
    Engine eng = Engine.newBuilder()
      .allowExperimentalOptions(true)
      .option(
        RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
        Paths.get("../../distribution/component").toFile().getAbsolutePath()
      ).build();
    Context ctx = Context.newBuilder()
      .engine(eng)
      .allowIO(true)
      .build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    org.junit.Assert.assertNotNull("Enso found: " + langs, langs.get("enso"));

    final URI facUri = new URI("memory://fac.enso");
    final Source facSrc = Source.newBuilder("enso", """
    fac : Number -> Number
    fac n =
        facacc : Number -> Number -> Number
        facacc n accumulator =
                stop = n <= 1
                if stop then accumulator else @Tail_Call facacc n-1 n*accumulator

        facacc n 1
    """, "fac.enso")
            .uri(facUri)
            .buildLiteral();

    var module = ctx.eval(facSrc);
    var facFn = module.invokeMember("eval_expression", "fac");
    final var dbg = Debugger.find(eng);
    final var values = new TreeSet<Integer>();
    try (var session = dbg.startSession((event) -> {
      final DebugValue accumulatorValue = findDebugValue(event, "accumulator");
      if (accumulatorValue != null) {
        final int accumulator = accumulatorValue.asInt();
        values.add(accumulator);
      }
      event.getSession().suspendNextExecution();
    })) {
      session.suspendNextExecution();
      var fac5 = facFn.execute(5);
      Assert.assertEquals("5!", 120, fac5.asInt());
    }
    assertEquals("Accumulator gets following values one by one", Set.of(1, 5, 20, 60, 120), values);
  }

  private static DebugValue findDebugValue(SuspendedEvent event, final String n) throws DebugException {
    for (var v : event.getTopStackFrame().getScope().getDeclaredValues()) {
      if (v.getName().contains(n)) {
        return v;
      }
    }
    return null;
  }
}
