package org.enso.shttp;

import com.sun.net.httpserver.*;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.stream.Stream;
import org.enso.shttp.auth.BasicAuthTestHandler;
import org.enso.shttp.auth.TokenAuthTestHandler;
import org.enso.shttp.cloud_mock.CloudRoot;
import sun.misc.Signal;
import sun.misc.SignalHandler;

import javax.net.ssl.SSLContext;

public class HTTPTestHelperServer {

  public static void main(String[] args) {
    if (args.length != 2) {
      System.err.println("Usage: http-test-helper <host> <port>");
      System.exit(1);
    }

    String host = args[0];
    HybridHTTPServer server = null;
    try {
      int port = Integer.parseInt(args[1]);
      server = new HybridHTTPServer(host, port, port + 1);
      setupEndpoints(server);

      final HybridHTTPServer server1 = server;
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

  private static void setupEndpoints(HybridHTTPServer server) throws URISyntaxException {
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
    setupFileServer(server);
  }

  private static void setupFileServer(HybridHTTPServer server) throws URISyntaxException {
    Path myRuntimeJar =
        Path.of(
                HTTPTestHelperServer.class
                    .getProtectionDomain()
                    .getCodeSource()
                    .getLocation()
                    .toURI())
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

  private static final String pathToWWW = "tools/http-test-helper/www-files";

  private static boolean looksLikeProjectRoot(Path path) {
    return Stream.of("build.sbt", "tools", "project", pathToWWW)
        .allMatch(p -> Files.exists(path.resolve(p)));
  }
}
