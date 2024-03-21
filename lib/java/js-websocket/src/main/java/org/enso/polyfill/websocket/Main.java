package org.enso.polyfill.websocket;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public class Main {

    private static final String DEMO_PATH = "/all-y-websocket.js";

    private Main() {
    }

    public static void main(String[] args) throws Exception {
        var demo = Main.class.getResource(DEMO_PATH);
        if (demo == null) {
            throw new IOException("Cannot find " + DEMO_PATH);
        }
        var commonJsRoot = new File(demo.toURI()).getParent();

        HostAccess hostAccess = HostAccess.newBuilder(HostAccess.EXPLICIT)
                .allowArrayAccess(true)
                .build();

        var b = Context.newBuilder("js")
                .allowIO(IOAccess.ALL)
                .allowHostAccess(hostAccess)
                .allowExperimentalOptions(true)
                .option("js.commonjs-require", "true")
                .option("js.commonjs-require-cwd", commonJsRoot);
        var chromePort = Integer.getInteger("inspectPort", -1);
        if (chromePort > 0) {
            b.option("inspect", ":" + chromePort);
        }

        try (var executor = Executors.newSingleThreadExecutor()) {
            var webSocketPolyfill = new WebSocketPolyfill(executor);
            var demoJs = Source.newBuilder("js", demo)
                    .mimeType("application/javascript+module")
                    .build();

            CompletableFuture
                    .supplyAsync(b::build, executor)
                    .thenAcceptAsync(ctx -> {
                        webSocketPolyfill.initialize(ctx);
                        ctx.eval(demoJs);
                    }, executor)
                    .get();

            System.out.println("Press enter to exit");
            System.in.read();
        }
    }

}
