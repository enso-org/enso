package org.enso.shttp;

import com.sun.net.httpserver.SimpleFileServer;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.Semaphore;
import java.util.stream.Stream;
import org.enso.shttp.auth.BasicAuthTestHandler;
import org.enso.shttp.auth.TokenAuthTestHandler;
import org.enso.shttp.cloud_mock.CloudAuthRenew;
import org.enso.shttp.cloud_mock.CloudMockSetup;
import org.enso.shttp.cloud_mock.CloudRoot;
import org.enso.shttp.cloud_mock.ExpiredTokensCounter;
import org.enso.shttp.test_helpers.*;
import sun.misc.Signal;
import sun.misc.SignalHandler;

public class HTTPTestHelperServer {

  public static void main(String[] args) {
    if (args.length < 2) {
      System.err.println("Usage: http-test-helper <host> <port> [additional test options]");
      System.exit(1);
    }
    String host = args[0];
    int port = Integer.parseInt(args[1]);
    String[] remainingArgs = Arrays.copyOfRange(args, 2, args.length);
    final Semaphore stopNotification = new Semaphore(0, false);
    HybridHTTPServer server = null;
    try {
      CloudMockSetup cloudMockSetup = CloudMockSetup.fromArgs(remainingArgs);
      server = createServer(host, port, null, true, cloudMockSetup);
    } catch (URISyntaxException | IOException e) {
      e.printStackTrace();
      System.exit(1);
    }

    SignalHandler stopServerHandler =
        (Signal sig) -> {
          System.out.println("Stopping server... (SIG" + sig.getName() + ")");
          stopNotification.release();
        };
    for (String signalName : List.of("TERM", "INT")) {
      Signal.handle(new Signal(signalName), stopServerHandler);
    }

    server.start();

    try {
      // Make sure the main thread is blocked for as long as the server is running.
      stopNotification.acquire();
    } catch (InterruptedException e) {
      System.out.println(
          "Server main thread was unexpectedly interrupted. The server will now stop.");
    } finally {
      server.stop();
    }
  }

  /**
   * Creates the server.
   *
   * @param executor An {@link Executor} for both HTTP and HTTPS servers. If {@code null}, the
   *     default executor is set, which runs the server on the thread that created the server.
   * @param withSSLServer Whether HTTPS server should be also be started along with HTTP server.
   * @return The created server
   */
  public static HybridHTTPServer createServer(
      String host,
      int port,
      Executor executor,
      boolean withSSLServer,
      CloudMockSetup cloudMockSetup)
      throws URISyntaxException, IOException {
    Path projectRoot = findProjectRoot();
    Path keyStorePath = projectRoot.resolve("tools/http-test-helper/target/keystore.jks");
    var server = new HybridHTTPServer(host, port, port + 1, keyStorePath, executor, withSSLServer);
    setupEndpoints(server, projectRoot, cloudMockSetup);
    return server;
  }

  private static void setupEndpoints(
      HybridHTTPServer server, Path projectRoot, CloudMockSetup cloudMockSetup) {
    for (HttpMethod method : HttpMethod.values()) {
      String path = "/" + method.toString().toLowerCase();
      server.addHandler(path, new TestHandler(method));
    }

    // HTTP helpers
    setupFileServer(server, projectRoot);
    server.addHandler("/test_headers", new HeaderTestHandler());
    server.addHandler("/test_token_auth", new TokenAuthTestHandler());
    server.addHandler("/test_basic_auth", new BasicAuthTestHandler());
    server.addHandler("/crash", new CrashingTestHandler());
    server.addHandler("/test_redirect", new RedirectTestHandler("/testfiles/js.txt"));
    server.addHandler("/test_download", new DownloadTestHandler());

    // Cloud mock
    if (cloudMockSetup != null) {
      var expiredTokensCounter = new ExpiredTokensCounter();
      server.addHandler("/COUNT-EXPIRED-TOKEN-FAILURES", expiredTokensCounter);
      CloudRoot cloudRoot = new CloudRoot(expiredTokensCounter, cloudMockSetup);
      server.addHandler(cloudRoot.prefix, cloudRoot);
      server.addHandler("/enso-cloud-auth-renew", new CloudAuthRenew());
    }

    // Data link helpers
    server.addHandler("/dynamic-datalink", new GenerateDataLinkHandler(true));
    server.addHandler("/dynamic.datalink", new GenerateDataLinkHandler(false));
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
