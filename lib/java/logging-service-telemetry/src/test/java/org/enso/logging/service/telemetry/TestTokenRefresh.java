package org.enso.logging.service.telemetry;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.enso.logging.service.common.TokenRefresher;
import org.enso.shttp.HTTPTestHelperServer;
import org.enso.shttp.HybridHTTPServer;
import org.enso.shttp.cloud_mock.CloudMockSetup;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

public class TestTokenRefresh {
  @Rule public final ConsumeLogs consumeLogs = new ConsumeLogs();
  @Rule public final RetryTestRule retry = new RetryTestRule(3);

  private static final int port = 8085;
  private static final URI refreshUri =
      URI.create("http://localhost:" + port + "/enso-cloud-auth-renew");
  private static final String refreshToken = "TEST-ENSO-REFRESH-caffee";
  private static final String clientId = "TEST-ENSO-CLIENT-ID";

  private HybridHTTPServer server;
  private ExecutorService serverExecutor;

  @Before
  public void initServer() throws URISyntaxException, IOException {
    serverExecutor = Executors.newSingleThreadExecutor();
    var cloudMockSetup = new CloudMockSetup(false);
    server =
        HTTPTestHelperServer.createServer("localhost", port, serverExecutor, false, cloudMockSetup);
    server.start();
  }

  @After
  public void stopServer() {
    server.stop();
    serverExecutor.shutdown();
  }

  @Test
  public void tokenRefresh() {
    var tokenRefresher = new TokenRefresher(refreshUri, clientId, refreshToken);
    var authData = tokenRefresher.fetchNewAccessToken();
    assertThat(authData, is(notNullValue()));
    assertThat(authData.accessToken(), containsString("TEST-RENEWED-0"));
  }
}
