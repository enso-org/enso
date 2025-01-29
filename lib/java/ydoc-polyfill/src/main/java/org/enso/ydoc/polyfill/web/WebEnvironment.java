package org.enso.ydoc.polyfill.web;

import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Function;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

/** Web polyfill environment. */
public final class WebEnvironment {

  public static final HostAccess.Builder defaultHostAccess =
      HostAccess.newBuilder(HostAccess.EXPLICIT).allowArrayAccess(true).allowBufferAccess(true);

  private WebEnvironment() {}

  public static void initialize(Context ctx, ScheduledExecutorService executor) {
    Function<java.net.URL, Value> eval =
        (url) -> {
          var src = Source.newBuilder("js", url).buildLiteral();
          return ctx.eval(src);
        };
    initialize(eval, executor);
  }

  public static void initialize(
      Function<java.net.URL, Value> eval, ScheduledExecutorService executor) {
    var performance = new Performance();
    performance.initialize(eval);

    var eventTarget = new EventTarget();
    eventTarget.initialize(eval);

    var eventEmitter = new EventEmitter();
    eventEmitter.initialize(eval);

    var timers = new Timers(executor);
    timers.initialize(eval);

    var crypto = new Crypto();
    crypto.initialize(eval);

    var encoding = new Util();
    encoding.initialize(eval);

    var abortController = new AbortController();
    abortController.initialize(eval);

    var zlib = new Zlib();
    zlib.initialize(eval);

    var webSocketPolyfill = new WebSocket(executor);
    webSocketPolyfill.initialize(eval);
  }

  public static Context.Builder createContext() {
    return createContext(defaultHostAccess.build());
  }

  public static Context.Builder createContext(HostAccess hostAccess) {
    var contextBuilder =
        Context.newBuilder("js").allowHostAccess(hostAccess).allowExperimentalOptions(true);

    var inspectPort = Integer.getInteger("inspectPort", -1);
    if (inspectPort > 0) {
      contextBuilder.option("inspect", ":" + inspectPort);
    }
    return contextBuilder;
  }
}
