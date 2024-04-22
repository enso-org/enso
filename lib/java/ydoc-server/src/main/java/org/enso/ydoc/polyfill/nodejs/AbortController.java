package org.enso.ydoc.polyfill.nodejs;

import java.util.Arrays;
import java.util.UUID;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

final class AbortController implements ProxyExecutable, Polyfill {

  private static final String RANDOM_UUID = "random-uuid";

  private static final String ABORT_CONTROLLER_JS = "abort-controller.js";

  AbortController() {}

  @Override
  public void initialize(Context ctx) {
    Source abortControllerJs =
        Source.newBuilder("js", AbortController.class.getResource(ABORT_CONTROLLER_JS))
            .buildLiteral();

    ctx.eval(abortControllerJs).execute(this);
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
