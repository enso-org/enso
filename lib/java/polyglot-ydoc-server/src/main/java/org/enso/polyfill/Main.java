package org.enso.polyfill;

import java.io.File;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;

import org.enso.polyfill.crypto.CryptoPolyfill;
import org.enso.polyfill.encoding.EncodingPolyfill;
import org.enso.polyfill.timers.TimersPolyfill;
import org.enso.polyfill.web.AbortControllerPolyfill;
import org.enso.polyfill.web.EventTargetPolyfill;
import org.enso.polyfill.websocket.WebSocketPolyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public class Main {

    private static final String YDOC_SERVER_PATH = "/dist/assets/ydocServer.js";

    private Main() {
    }

    public static void main(String[] args) throws Exception {
        var ydoc = ClasspathResource.createTempFile(YDOC_SERVER_PATH);
        var commonJsRoot = new File(ydoc).getParent();

        HostAccess hostAccess = HostAccess.newBuilder(HostAccess.EXPLICIT)
                .allowArrayAccess(true)
                .build();

        var b = Context.newBuilder("js")
                .allowIO(IOAccess.ALL)
                .allowHostAccess(hostAccess)
                .allowExperimentalOptions(true)
                .option("js.commonjs-require", "true")
                .option("js.commonjs-require-cwd", commonJsRoot);
        var chromePort = 34567;// Integer.getInteger("inspectPort", -1);
        if (chromePort > 0) {
            b.option("inspect", ":" + chromePort);
        }

        try (var executor = Executors.newSingleThreadExecutor()) {
            var ydocJs = Source.newBuilder("js", ydoc.toURL())
                    .mimeType("application/javascript+module")
                    .build();

            CompletableFuture
                    .supplyAsync(b::build, executor)
                    .thenAcceptAsync(ctx -> {
                        var eventTarget = new EventTargetPolyfill(executor);
                        eventTarget.initialize(ctx);

                        var timers = new TimersPolyfill(executor);
                        timers.initialize(ctx);

                        var crypto = new CryptoPolyfill();
                        crypto.initialize(ctx);

                        var encoding = new EncodingPolyfill();
                        encoding.initialize(ctx);

                        var abortController = new AbortControllerPolyfill();
                        abortController.initialize(ctx);

                        var webSocketPolyfill = new WebSocketPolyfill(executor);
                        webSocketPolyfill.initialize(ctx);

                        ctx.eval(ydocJs);
                    }, executor)
                    .get();

            System.out.println("Press enter to exit");
            System.in.read();
        }
    }
}
