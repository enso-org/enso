package org.enso.shttp;

import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.SimpleFileServer;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import sun.misc.Signal;
import sun.misc.SignalHandler;

public class SimpleHTTPBin {

  private final HttpServer server;
  private final State state;

  public SimpleHTTPBin(String hostname, int port) throws IOException {
    InetSocketAddress address = new InetSocketAddress(hostname, port);
    server = HttpServer.create(address, 0);
    server.setExecutor(null);
    state = new State();
  }

  public void start() {
    server.start();
    state.start();

    try {
      while (state.isRunning()) {
        Thread.sleep(1000);
      }
    } catch (InterruptedException e) {
      e.printStackTrace();
    } finally {
      System.out.println("Finalizing server...");
      server.stop(3);
      System.out.println("Server stopped.");
    }
  }

  public void stop() {
    state.stop();
  }

  public void addHandler(String path, HttpHandler handler) {
    server.createContext(path, handler);
  }

  public static void main(String[] args) {
    if (args.length != 2) {
      System.err.println("Usage: SimpleHTTPBin <host> <port>");
      System.exit(1);
    }

    String host = args[0];
    SimpleHTTPBin server = null;
    try {
      int port = Integer.valueOf(args[1]);
      server = new SimpleHTTPBin(host, port);
      setupEndpoints(server);

      final SimpleHTTPBin server1 = server;
      SignalHandler stopServerHandler =
          (Signal sig) -> {
            System.out.println("Stopping server... (interrupt)");
            server1.stop();
          };
      for (String signalName : List.of("TERM", "INT")) {
        Signal.handle(new Signal(signalName), stopServerHandler);
      }
      server.start();
    } catch (IOException | URISyntaxException e) {
      e.printStackTrace();
    } finally {
      if (server != null) {
        server.stop();
      }
    }
  }

  private class State {
    private boolean running = false;

    void stop() {
      running = false;
    }

    void start() {
      running = true;
    }

    boolean isRunning() {
      return running;
    }
  }

  private static void setupEndpoints(SimpleHTTPBin server) throws URISyntaxException {
    for (HttpMethod method : HttpMethod.values()) {
      String path = "/" + method.toString().toLowerCase();
      server.addHandler(path, new TestHandler(method));
    }

    server.addHandler("/test_headers", new HeaderTestHandler());
    server.addHandler("/test_token_auth", new TokenAuthTestHandler());
    server.addHandler("/test_basic_auth", new BasicAuthTestHandler());
    server.addHandler("/crash", new CrashingTestHandler());
    setupFileServer(server);
  }

  private static void setupFileServer(SimpleHTTPBin server) throws URISyntaxException {
    Path myRuntimeJar =
        Path.of(SimpleHTTPBin.class.getProtectionDomain().getCodeSource().getLocation().toURI())
            .toAbsolutePath();
    Path projectRoot = findProjectRoot(myRuntimeJar);
    Path testFilesRoot = projectRoot.resolve(pathToWWW);
    System.out.println("Serving files from directory " + testFilesRoot);
    server.addHandler("/testfiles", SimpleFileServer.createFileHandler(testFilesRoot));
  }

  private static Path findProjectRoot(Path startingPoint) {
    if (looksLikeProjectRoot(startingPoint)) {
      return startingPoint;
    } else {
      Path parent = startingPoint.getParent();
      if (parent == null) {
        throw new RuntimeException("Could not find project root");
      }

      return findProjectRoot(parent);
    }
  }

  private static final String pathToWWW = "tools/simple-httpbin/www-files";

  private static boolean looksLikeProjectRoot(Path path) {
    return Stream.of("build.sbt", "tools", "project", pathToWWW)
        .allMatch(p -> Files.exists(path.resolve(p)));
  }
}
