package org.enso.ydoc.server;

import io.helidon.common.buffers.BufferData;
import io.helidon.http.Status;
import io.helidon.webclient.api.WebClient;
import io.helidon.webclient.websocket.WsClient;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.api.YjsChannel;
import org.enso.ydoc.polyfill.web.WebEnvironment;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class YdocTest {

  private static final int WEB_SERVER_PORT = 44556;
  private static final String YDOC_URL = "ws://localhost:5976/project/";
  private static final String HEALTHCHECK_URL = "http://localhost:5976/_health";
  private static final String WEB_SERVER_URL = "ws://127.0.0.1:" + WEB_SERVER_PORT;

  private static final Logger log = LoggerFactory.getLogger(YdocTest.class);

  private Ydoc ydoc;
  private ExecutorService executor;

  private static String ydocUrl(String doc) {
    return YDOC_URL + doc + "?ls=" + WEB_SERVER_URL + "&data=" + WEB_SERVER_URL;
  }

  @Before
  public void setup() throws Exception {
    executor =
        Executors.newSingleThreadExecutor(
            (r) -> {
              var t = new Thread(r);
              t.setName("ydoc-thread");
              return t;
            });
  }

  @After
  public void tearDown() throws Exception {
    if (executor != null) {
      executor.shutdownNow();
      executor.awaitTermination(3, TimeUnit.SECONDS);
    }
    if (ydoc != null) {
      ydoc.close();
    }
  }

  @Test(timeout = 60000)
  public void start() throws Exception {
    var queue = new LinkedBlockingQueue<BufferData>();
    var jsonOnConnectLatch = new CountDownLatch(1);
    var binaryOnConnectLatch = new CountDownLatch(1);

    YjsChannel.Server jsonCallbacks =
        (YjsChannel channel) -> {
          log.debug("Json onConnect called with channel: {}", channel);
          jsonOnConnectLatch.countDown();
        };

    YjsChannel.Server binaryCallbacks =
        (YjsChannel channel) -> {
          log.debug("Binary onConnect called with channel: {}", channel);
          binaryOnConnectLatch.countDown();
        };

    executor.submit(
        () -> {
          try {
            var hostAccess =
                WebEnvironment.defaultHostAccess
                    .allowImplementations(YjsChannel.class)
                    .allowPublicAccess(true);
            ydoc =
                Ydoc.builder()
                    .hostAccess(hostAccess.build())
                    .jsonChannelCallbacks(jsonCallbacks)
                    .binaryChannelCallbacks(binaryCallbacks)
                    .build();
            ydoc.start();
          } catch (IOException e) {
            e.printStackTrace();
            Assert.fail("Ydoc.start()");
          }
        });

    var connected = false;
    var ws = WsClient.builder().build();
    while (!connected) {
      try {
        ws.connect(ydocUrl("index"), new DashboardConnection(queue));
        connected = true;
      } catch (Exception _ignore) {
      }
    }

    Assert.assertTrue("Client should be connected", connected);

    var jsonOnConnectCalled = jsonOnConnectLatch.await(30, TimeUnit.SECONDS);
    var binaryOnConnectCalled = binaryOnConnectLatch.await(30, TimeUnit.SECONDS);
    Assert.assertTrue(
        "Json onConnect callback should be called after client connects", jsonOnConnectCalled);
    Assert.assertTrue(
        "Binary onConnect callback should be called after client connects", binaryOnConnectCalled);

    var waitMinute = Duration.ofMinutes(1);
    var http = WebClient.create();
    var healthcheckResponse =
        http.get(HEALTHCHECK_URL).readContinueTimeout(waitMinute).readTimeout(waitMinute).request();
    Assert.assertEquals(Status.OK_200, healthcheckResponse.status());
  }

  private static final class DashboardConnection implements WsListener {

    private static final Logger log = LoggerFactory.getLogger(DashboardConnection.class);

    private final BlockingQueue<BufferData> messages;

    private DashboardConnection(BlockingQueue<BufferData> messages) {
      this.messages = messages;
    }

    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      log.debug("Got message\n{}", buffer.debugDataHex());

      messages.add(buffer);
    }

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      log.error("Got unexpected message [{}].", text);
    }
  }
}
