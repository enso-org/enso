package org.enso.ydoc.polyfill.web;

import java.util.Arrays;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

/**
 * Implements the <a href="https://nodejs.org/api/perf_hooks.html">Performance measurement</a>
 * Node.js API.
 */
final class Performance implements ProxyExecutable, Polyfill {

  private static final String NOW = "now";

  private static final String PERFORMANCE_JS = "performance.js";

  Performance() {}

  @Override
  public void initialize(Context ctx) {
    Source performanceJs =
        Source.newBuilder("js", Performance.class.getResource(PERFORMANCE_JS)).buildLiteral();

    ctx.eval(performanceJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();
    System.err.println(command + " " + Arrays.toString(arguments));

    return switch (command) {
      case NOW -> System.currentTimeMillis();

      default -> throw new IllegalStateException(command);
    };
  }
}
