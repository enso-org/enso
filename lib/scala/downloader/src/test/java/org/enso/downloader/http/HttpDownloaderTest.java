package org.enso.downloader.http;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertTrue;

import com.sun.net.httpserver.HttpExchange;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.enso.cli.task.ProgressListener;
import org.enso.cli.task.TaskProgress;
import org.enso.shttp.HTTPTestHelperServer;
import org.enso.shttp.HybridHTTPServer;
import org.enso.shttp.SimpleHttpHandler;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import scala.Option;
import scala.util.Try;

public class HttpDownloaderTest {

  private static final int port = 8090;
  private static final String TEXTS_FOO_URI = "http://localhost:" + port + "/texts/foo";
  public static final String REDIRECT_URI = "/redirect";
  private static HybridHTTPServer server;
  private static ExecutorService serverExecutor;

  @BeforeClass
  public static void initServer() throws URISyntaxException, IOException {
    serverExecutor = Executors.newSingleThreadExecutor();
    server = HTTPTestHelperServer.createServer("localhost", port, serverExecutor, false);
    server.addHandler("/texts", new TextHandler());
    server.addHandler("/redirect", new RedirectHandler());
    server.addHandler("/files", new BigFileHandler());
    server.start();
  }

  @AfterClass
  public static void stopServer() {
    server.stop();
    serverExecutor.shutdown();
  }

  @Test
  public void fetchStringTest() throws URISyntaxException {
    var uri = new URI(TEXTS_FOO_URI);
    var task = HTTPDownload.fetchString(uri);
    var resp = task.force();
    assertThat(resp.content(), containsString("Hello"));
  }

  @Test
  public void handleRedirectTest() throws URISyntaxException {
    var uriStr = "http://localhost:" + port + REDIRECT_URI;
    var uri = new URI(uriStr);
    var task = HTTPDownload.fetchString(uri);
    var resp = task.force();
    assertThat(resp.content(), containsString("Hello"));
  }

  @Test
  public void downloadSmallFileTest() throws Exception {
    var uriStr = "http://localhost:" + port + "/get";
    var uri = new URI(uriStr);
    Path dest = Files.createTempFile("enso-downloader-test", ".json");
    try {
      var task = HTTPDownload.download(uri, dest);
      var resp = task.force();
      assertThat(resp.toFile().exists(), is(true));
      assertThat(resp.toFile(), is(dest.toFile()));
    } finally {
      Files.deleteIfExists(dest);
    }
  }

  @Test
  public void handleResourceNotFound() throws Exception {
    var uri = new URI("http://localhost:" + port + "/not-found-FOO-BAR");
    var task = HTTPDownload.fetchString(uri);
    var resp = TaskProgress.waitForTask(task);
    assertThat(resp.isFailure(), is(true));
    var exception = resp.failed().get();
    assertThat(exception, instanceOf(ResourceNotFound.class));
  }

  @Test
  public void downloadBigFileWithProgress() throws Exception {
    var uriStr = "http://localhost:" + port + "/files/big.tgz";
    var uri = new URI(uriStr);
    Path dest = Files.createTempFile("enso-downloader-test", ".tgz");
    final int[] progressUpdateCalls = {0};
    final int[] doneCalls = {0};
    var task = HTTPDownload.download(uri, dest);
    var progressListener =
        new ProgressListener<Path>() {
          @Override
          public void progressUpdate(long done, Option<Object> total) {
            progressUpdateCalls[0]++;
            assertThat(total.isDefined(), is(true));
            assertThat(total.get(), instanceOf(Long.class));
            long reportedTotal = (Long) total.get();
            assertThat(reportedTotal, is(BigFileHandler.BIG_FILE_SIZE));
            assertThat(
                "Should send and report just chunk, not the whole file",
                done,
                lessThan(reportedTotal));
          }

          @Override
          public void done(Try<Path> result) {
            doneCalls[0]++;
            assertTrue(result.isSuccess());
            assertThat(result.get(), is(dest));
          }
        };
    task.addProgressListener(progressListener);
    var resp = task.force();
    assertThat(resp.toFile().exists(), is(true));
    assertThat("Done was called exactly once", doneCalls[0], is(1));
    assertThat(
        "Progress reported was called at least once", progressUpdateCalls[0], greaterThan(0));
    Files.deleteIfExists(dest);
  }

  @Test
  public void fetchStringWithProgress() throws URISyntaxException {
    var uri = new URI(TEXTS_FOO_URI);
    var task = HTTPDownload.fetchString(uri);
    final int[] progressUpdateCalls = {0};
    final int[] doneCalls = {0};
    var progressListener =
        new ProgressListener<APIResponse>() {
          @Override
          public void progressUpdate(long done, Option<Object> total) {
            progressUpdateCalls[0]++;
            assertThat(total.isDefined(), is(true));
            assertThat(total.get(), instanceOf(Long.class));
            long reportedTotal = (Long) total.get();
            assertThat(reportedTotal, greaterThan(0L));
          }

          @Override
          public void done(Try<APIResponse> result) {
            doneCalls[0]++;
            assertTrue(result.isSuccess());
            assertThat(result.get(), notNullValue());
            assertThat(result.get().content(), containsString("Hello"));
          }
        };
    task.addProgressListener(progressListener);
    var resp = task.force();
    assertThat(resp.content(), containsString("Hello"));
    assertThat("Done was called exactly once", doneCalls[0], is(1));
    assertThat(
        "Progress reported was called at least once", progressUpdateCalls[0], greaterThan(0));
  }

  private static class TextHandler extends SimpleHttpHandler {
    @Override
    protected void doHandle(HttpExchange exchange) throws IOException {
      if (exchange.getRequestURI().toString().equals("/texts/foo")) {
        sendResponse(200, "Hello, world!", exchange);
      } else {
        exchange.sendResponseHeaders(404, -1);
      }
    }
  }

  private static class RedirectHandler extends SimpleHttpHandler {
    @Override
    protected void doHandle(HttpExchange exchange) throws IOException {
      if (exchange.getRequestURI().toString().equals(REDIRECT_URI)) {
        exchange.getResponseHeaders().add("Location", TEXTS_FOO_URI);
        exchange.sendResponseHeaders(302, -1);
      } else {
        exchange.sendResponseHeaders(404, -1);
      }
    }
  }

  private static class BigFileHandler extends SimpleHttpHandler {
    private static final int CHUNK_SIZE = 1024;
    private static final long BIG_FILE_SIZE = 10 * CHUNK_SIZE;
    private static final byte[] BIG_FILE_BYTES = new byte[Math.toIntExact(BIG_FILE_SIZE)];

    static {
      var rnd = new Random(42);
      rnd.nextBytes(BIG_FILE_BYTES);
    }

    @Override
    protected void doHandle(HttpExchange exchange) throws IOException {
      if (exchange.getRequestURI().toString().equals("/files/big.tgz")) {
        long chunks = BIG_FILE_SIZE / CHUNK_SIZE;
        exchange.getResponseHeaders().set("Content-Length", Long.toString(BIG_FILE_SIZE));
        exchange.getResponseHeaders().set("Content-Type", "application/x-gzip");
        // Set responseLength to 0 to indicate that the response length is unknown
        // and force chunking the response.
        exchange.sendResponseHeaders(200, 0);
        try (var os = exchange.getResponseBody()) {
          for (int i = 0; i < chunks; i++) {
            os.write(BIG_FILE_BYTES, i * CHUNK_SIZE, CHUNK_SIZE);
            os.flush();
          }
        }
      } else {
        exchange.sendResponseHeaders(404, -1);
      }
    }
  }
}
