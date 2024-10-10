package org.enso.ydoc.polyfill.web;

import java.util.UUID;
import org.enso.ydoc.Polyfill;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Implements the <a href="https://nodejs.org/api/crypto.html">Crypto</a> Node.js interface. */
final class Crypto implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(Crypto.class);

  private static final String RANDOM_UUID = "random-uuid";

  private static final String CRYPTO_JS = "crypto.js";

  @Override
  public void initialize(Context ctx) {
    Source jsSource = Source.newBuilder("js", getClass().getResource(CRYPTO_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case RANDOM_UUID -> UUID.randomUUID().toString();

      default -> throw new IllegalStateException(command);
    };
  }
}
