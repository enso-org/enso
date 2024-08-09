package org.enso.ydoc.polyfill.web;

import org.enso.ydoc.Polyfill;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements the <a
 * href="https://nodejs.org/api/globals.html#class-abortcontroller">AbortController</a> Node.js
 * interface.
 */
final class AbortController implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(AbortController.class);
  private static final String ABORT_CONTROLLER_JS = "abort-controller.js";

  @Override
  public void initialize(Context ctx) {
    Source jsSource =
        Source.newBuilder("js", getClass().getResource(ABORT_CONTROLLER_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    throw new IllegalStateException(command);
  }
}
