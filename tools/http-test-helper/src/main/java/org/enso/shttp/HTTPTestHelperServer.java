package org.enso.shttp;

import com.sun.net.httpserver.SimpleFileServer;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.concurrent.Executor;
import java.util.concurrent.Semaphore;
import java.util.stream.Stream;
import org.enso.shttp.auth.BasicAuthTestHandler;
import org.enso.shttp.auth.TokenAuthTestHandler;
import org.enso.shttp.cloud_mock.CloudAuthRenew;
import org.enso.shttp.cloud_mock.CloudMockSetup;
import org.enso.shttp.cloud_mock.CloudRoot;
import org.enso.shttp.cloud_mock.ExpiredTokensCounter;
import org.enso.shttp.test_helpers.CrashingTestHandler;
import org.enso.shttp.test_helpers.DownloadTestHandler;
import org.enso.shttp.test_helpers.GenerateDataLinkHandler;
import org.enso.shttp.test_helpers.HeaderTestHandler;
import org.enso.shttp.test_helpers.RedirectTestHandler;
import org.enso.shttp.test_helpers.TestHandler;

public class HTTPTestHelperServer {

  public static void main(String[] args) {
    if (args.length < 2) {
      System.err.println("Usage: http-test-helper <host> <port> [additional test options]");
      System.exit(1);
    }
    String host = args[0];
    int port = Integer.parseInt(args[1]);
    String[] remainingArgs = Arrays.copyOfRange(args, 2, args.length);
    HybridHTTPServer server = null;
    try {
      CloudMockSetup cloudMockSetup = CloudMockSetup.fromArgs(remainingArgs);
      server = createServer(host, port, null, true, cloudMockSetup);
    } catch (URISyntaxException | IOException e) {
      e.printStackTrace();
      System.exit(1);
    }

    Semaphore semaphore = new Semaphore(0);
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread() {
              public void run() {
                semaphore.release();
              }
            });
    server.start();
    System.out.println("Server started.");
    try {
      semaphore.acquire();
      System.out.println("Shutting down...");
    } catch (InterruptedException e) {
      System.err.println("Shutting down abruptly...");
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
      server.addCloudRoot(cloudRoot);
      server.addHandler(cloudRoot.prefix, cloudRoot);
      var cloudAuthRenew = new CloudAuthRenew();
      server.addHandler("/enso-cloud-auth-renew", cloudAuthRenew);
      server.addCloudAuthRenew(cloudAuthRenew);
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
