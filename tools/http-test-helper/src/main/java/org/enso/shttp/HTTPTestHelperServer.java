package org.enso.shttp;

import com.sun.net.httpserver.SimpleFileServer;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import org.enso.shttp.auth.BasicAuthTestHandler;
import org.enso.shttp.auth.TokenAuthTestHandler;
import org.enso.shttp.cloud_mock.CloudRoot;
import sun.misc.Signal;
import sun.misc.SignalHandler;

public class HTTPTestHelperServer {

  public static void main(String[] args) {
    if (args.length != 2) {
      System.err.println("Usage: http-test-helper <host> <port>");
      System.exit(1);
    }

    String host = args[0];
    HybridHTTPServer server = null;
    try {
      Path projectRoot = findProjectRoot();
      Path keyStorePath = projectRoot.resolve("tools/http-test-helper/target/keystore.jks");
      int port = Integer.parseInt(args[1]);
      server = new HybridHTTPServer(host, port, port + 1, keyStorePath);
      setupEndpoints(server, projectRoot);

      final HybridHTTPServer server1 = server;
      SignalHandler stopServerHandler =
          (Signal sig) -> {
            System.out.println("Stopping server... (SIG" + sig.getName() + ")");
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

  private static void setupEndpoints(HybridHTTPServer server, Path projectRoot)
      throws URISyntaxException {
    for (HttpMethod method : HttpMethod.values()) {
      String path = "/" + method.toString().toLowerCase();
      server.addHandler(path, new TestHandler(method));
    }

    server.addHandler("/test_headers", new HeaderTestHandler());
    server.addHandler("/test_token_auth", new TokenAuthTestHandler());
    server.addHandler("/test_basic_auth", new BasicAuthTestHandler());
    server.addHandler("/crash", new CrashingTestHandler());
    CloudRoot cloudRoot = new CloudRoot();
    server.addHandler(cloudRoot.prefix, cloudRoot);
    setupFileServer(server, projectRoot);
  }

  private static void setupFileServer(HybridHTTPServer server, Path projectRoot) {
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

  private static Path findProjectRoot() throws URISyntaxException {
    Path myRuntimeJar =
        Path.of(
                HTTPTestHelperServer.class
                    .getProtectionDomain()
                    .getCodeSource()
                    .getLocation()
                    .toURI())
            .toAbsolutePath();
    return findProjectRoot(myRuntimeJar);
  }

  private static final String pathToWWW = "tools/http-test-helper/www-files";

  private static boolean looksLikeProjectRoot(Path path) {
    return Stream.of("build.sbt", "tools", "project", pathToWWW)
        .allMatch(p -> Files.exists(path.resolve(p)));
  }
}
