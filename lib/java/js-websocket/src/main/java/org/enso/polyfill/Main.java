package org.enso.polyfill;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;

import org.enso.polyfill.websocket.WebSocketPolyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.io.IOAccess;

public class Main {

    private static final String YDOC_SERVER_PATH = "/ydoc-server-bundle.js";
    private static final String WASM_PATH = "/153299079965dcf860b5.wasm";

    private Main() {
    }

    public static void main(String[] args) throws Exception {
        ClasspathResource.createTempFile(WASM_PATH);
        var demo = ClasspathResource.createTempFile(YDOC_SERVER_PATH);
        if (demo == null) {
            throw new IOException("Cannot find " + YDOC_SERVER_PATH);
        }
        var commonJsRoot = new File(demo).getParent();

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
            var demoJs = Source.newBuilder("js", demo.toURL())
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
