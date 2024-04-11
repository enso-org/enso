package org.enso.ydoc.polyfill;

import java.util.concurrent.ExecutorService;
import org.graalvm.polyglot.Context;

public final class Platform {

  public static void initialize(Context ctx, ExecutorService executor) {
    var performance = new Performance();
    performance.initialize(ctx);

    var eventTarget = new EventTarget(executor);
    eventTarget.initialize(ctx);

    var timers = new Timers(executor);
    timers.initialize(ctx);

    var crypto = new Crypto();
    crypto.initialize(ctx);

    var encoding = new Encoding();
    encoding.initialize(ctx);

    var abortController = new AbortController();
    abortController.initialize(ctx);

    var webSocketPolyfill = new WebSocket(executor);
    webSocketPolyfill.initialize(ctx);
  }
}
