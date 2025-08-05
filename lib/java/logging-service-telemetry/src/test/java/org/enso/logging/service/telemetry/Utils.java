package org.enso.logging.service.telemetry;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import org.enso.logging.service.common.Credentials;
import org.enso.logging.service.common.LogJob;
import org.enso.shttp.HTTPTestHelperServer;
import org.enso.shttp.HybridHTTPServer;
import org.enso.shttp.cloud_mock.CloudMockSetup;

final class Utils {
  private Utils() {}

  static URI logUri(int port) {
    try {
      return new URI("http://localhost:" + port + "/enso-cloud-mock/logs");
    } catch (URISyntaxException e) {
      throw new AssertionError(e);
    }
  }

  static URI refreshUri(int port) {
    try {
      return new URI("http://localhost:" + port + "/enso-cloud-auth-renew");
    } catch (URISyntaxException e) {
      throw new AssertionError(e);
    }
  }

  static HybridHTTPServer createMockServer(int port) {
    var serverExecutor = Executors.newSingleThreadExecutor();
    var cloudMockSetup = new CloudMockSetup(false);
    HybridHTTPServer server = null;
    try {
      server =
          HTTPTestHelperServer.createServer(
              "localhost", port, serverExecutor, false, cloudMockSetup);
    } catch (URISyntaxException | IOException e) {
      throw new AssertionError(e);
    }
    return server;
  }

  static Credentials mockCredentials(int port) {
    var expireAt = ZonedDateTime.now().plusYears(1).format(DateTimeFormatter.ISO_INSTANT);
    var refreshUrl = refreshUri(port).toString();
    var accessToken = "TEST-ENSO-TOKEN-caffee";
    var refreshToken = "TEST-ENSO-REFRESH-caffee";
    var clientId = "TEST-ENSO-CLIENT-ID";
    return new Credentials(clientId, accessToken, refreshToken, refreshUrl, expireAt);
  }

  static void assertCompletedSuccessfully(List<LogJob> jobs) {
    for (LogJob logJob : jobs) {
      try {
        logJob.completionNofitication().get();
      } catch (InterruptedException e) {
        throw new AssertionError("Should not be interrupted", e);
      } catch (ExecutionException e) {
        throw new AssertionError("Should not fail", e);
      }
    }
  }

  static void assertCompletedSuccessfully(LogJob job) {
    assertCompletedSuccessfully(List.of(job));
  }
}
