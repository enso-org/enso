package org.enso.ydoc.polyfill.web;

import java.util.Arrays;
import java.util.UUID;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

/** Implements the <a href="https://nodejs.org/api/crypto.html">Crypto</a> Node.js interface. */
final class Crypto implements ProxyExecutable, Polyfill {

  private static final String RANDOM_UUID = "random-uuid";

  private static final String CRYPTO_JS = "crypto.js";

  Crypto() {}

  @Override
  public void initialize(Context ctx) {
    Source cryptoJs = Source.newBuilder("js", Crypto.class.getResource(CRYPTO_JS)).buildLiteral();

    ctx.eval(cryptoJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();
    System.err.println(command + " " + Arrays.toString(arguments));

    return switch (command) {
      case RANDOM_UUID -> UUID.randomUUID().toString();

      default -> throw new IllegalStateException(command);
    };
  }
}
