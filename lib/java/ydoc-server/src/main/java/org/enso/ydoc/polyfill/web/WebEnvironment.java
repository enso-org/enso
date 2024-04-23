package org.enso.ydoc.polyfill.web;

import java.util.concurrent.ExecutorService;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;

/** Web polyfill environment. */
public final class WebEnvironment {

  public static final HostAccess.Builder defaultHostAccess =
      HostAccess.newBuilder(HostAccess.EXPLICIT).allowArrayAccess(true).allowBufferAccess(true);

  private WebEnvironment() {}

  public static void initialize(Context ctx, ExecutorService executor) {
    var performance = new Performance();
    performance.initialize(ctx);

    var eventTarget = new EventTarget();
    eventTarget.initialize(ctx);

    var eventEmitter = new EventEmitter();
    eventEmitter.initialize(ctx);

    var timers = new Timers(executor);
    timers.initialize(ctx);

    var crypto = new Crypto();
    crypto.initialize(ctx);

    var encoding = new Util();
    encoding.initialize(ctx);

    var abortController = new AbortController();
    abortController.initialize(ctx);

    var webSocketPolyfill = new WebSocket(executor);
    webSocketPolyfill.initialize(ctx);
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
