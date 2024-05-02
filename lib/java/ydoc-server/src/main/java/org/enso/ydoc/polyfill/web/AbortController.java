package org.enso.ydoc.polyfill.web;

import java.util.UUID;
import org.enso.ydoc.polyfill.Arguments;
import org.enso.ydoc.polyfill.PolyfillBase;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements the <a
 * href="https://nodejs.org/api/globals.html#class-abortcontroller">AbortController</a> Node.js
 * interface.
 */
final class AbortController extends PolyfillBase implements ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(AbortController.class);
  private static final String ABORT_CONTROLLER_JS = "abort-controller.js";

  AbortController() {
    super(ABORT_CONTROLLER_JS);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    throw new IllegalStateException(command);
  }
}
