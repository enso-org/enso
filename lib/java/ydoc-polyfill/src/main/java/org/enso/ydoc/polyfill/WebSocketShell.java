package org.enso.ydoc.polyfill;

import java.io.File;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.stream.Stream;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

/**
 * Helper main class to execute provided JavaScript file with the {@code org.enso.ydoc.polyfill}
 * emulation. The emulation exposes <a href="http://nodejs.org">node.js</a> compatible environment
 * with <a href="https://nodejs.org/en/learn/getting-started/websocket">WebSocket</a> and <a
 * href="https://github.com/websockets/ws">WebSocketServer</a> objects being emulated. The goal of
 * the emulation isn't 100% compatible replacement (yet), but (only) to execute enough of <a
 * href="https://github.com/yjs/yjs">Y.js</a> code for Enso purposes.
 *
 * <h3>Usage</h3>
 *
 * Get built version of Enso engine distribution. Or use <code>sbt buildEngineDistribution</code> to
 * build it yourself from the Enso sources. There is {@code component} directory in the
 * installation. Use the GraalVM's Java command to execute:
 *
 * <pre>
 * $ graalvm/bin/java
 *    -p enso-engine-*?/enso-*?/component/
 *    -m org.enso.ydoc.polyfill/org.enso.ydoc.polyfill.WebSocketShell
 * </pre>
 *
 * The {@code enso-engine-*?/enso-*?/component/} represents path to the @{code component} directory
 * which needs to be adjusted to proper path on each operating system. All lines may need to be
 * concatenated into a single line.
 */
final class WebSocketShell implements AutoCloseable {
  private final Context ctx;
  private final ScheduledExecutorService executor;

  private WebSocketShell(boolean inspect) throws Exception {
    var access =
        HostAccess.newBuilder(HostAccess.EXPLICIT).allowBufferAccess(true).allowArrayAccess(true);
    var b = Context.newBuilder().allowExperimentalOptions(true).allowHostAccess(access.build());
    if (inspect) {
      b = b.option("inspect", "true");
    }
    this.ctx = b.build();
    this.executor =
        Executors.newSingleThreadScheduledExecutor(
            ((runnable) -> {
              return new Thread(runnable, "js-main-thread");
            }));
    WebEnvironment.initialize(ctx, executor);
  }

  final Future<Value> eval(File code) throws Exception {
    var src = Source.newBuilder("js", code).build();
    var res =
        executor.submit(
            () -> {
              try {
                return ctx.eval(src);
              } catch (Throwable t) {
                t.printStackTrace();
                throw t;
              }
            });
    return res;
  }

  final Future<Value> eval(String line) throws Exception {
    var res =
        executor.submit(
            () -> {
              return ctx.eval("js", line);
            });
    return res;
  }

  @Override
  public void close() throws Exception {
    executor.shutdown();
    ctx.close(true);
  }

  public static void main(String[] params) throws Exception {
    var args = List.of(params);
    var inspect = args.contains("--inspect");
    var files =
        args.stream()
            .filter(arg -> !arg.startsWith("--"))
            .map(path -> new File(path))
            .flatMap(
                file -> {
                  if (file.exists()) {
                    return Stream.of(file);
                  } else {
                    System.err.printf("File %s doesn't exist\n", file);
                    return Stream.empty();
                  }
                })
            .toList();

    if (files.size() != 1) {
      System.err.println("Executes provided JavaScript file in WebSocket enabled environment.");
      System.err.println("Usage: [--inspect] path_to_script.js");
      System.exit(1);
    }

    try (var client = new WebSocketShell(inspect)) {
      var code = files.get(0);
      var res = client.eval(code);
      System.out.printf("Script %s evaluated to %s\n", code, res);
      var console = System.console();
      for (int i = 0; i < 10; i++) {
        var line = console.readLine("js> ");
        if (line == null) {
          break;
        }
        res = client.eval(line);
        System.out.println(res.get());
      }
      System.out.printf("Exiting with %s", res);
    }
  }
}
