package org.enso.ydoc.polyfill.web;

import org.enso.ydoc.polyfill.Arguments;
import org.enso.ydoc.polyfill.PolyfillBase;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements the <a href="https://nodejs.org/api/perf_hooks.html">Performance measurement</a>
 * Node.js API.
 */
final class Performance extends PolyfillBase implements ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(Performance.class);

  private static final String NOW = "now";

  private static final String PERFORMANCE_JS = "performance.js";

  Performance() {
    super(PERFORMANCE_JS);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case NOW -> System.currentTimeMillis();

      default -> throw new IllegalStateException(command);
    };
  }
}
