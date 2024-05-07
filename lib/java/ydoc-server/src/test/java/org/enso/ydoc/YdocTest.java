package org.enso.ydoc;

import io.helidon.common.buffers.BufferData;
import io.helidon.webclient.websocket.WsClient;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class YdocTest {

  private static final int WEB_SERVER_PORT = 44556;
  private static final String YDOC_URL = "ws://localhost:1234/project/index";
  private static final String WEB_SERVER_URL = "ws://127.0.0.1:" + WEB_SERVER_PORT;

  private Ydoc ydoc;
  private ExecutorService webServerExecutor;
  private WebServer ls;

  private static WebServer startWebSocketServer(ExecutorService executor) {
    var routing = WsRouting.builder().endpoint("/", new WebServerWsListener());
    var ws =
        WebServer.builder().host("localhost").port(WEB_SERVER_PORT).addRouting(routing).build();

    executor.submit(ws::start);

    return ws;
  }

  @Before
  public void setup() {
    webServerExecutor = Executors.newSingleThreadExecutor();
    ls = startWebSocketServer(webServerExecutor);
    ydoc = new Ydoc();
  }

  @After
  public void tearDown() throws Exception {
    ls.stop();
    webServerExecutor.close();
    ydoc.close();
  }

  @Test
  public void connect() throws Exception {
    var queue = new LinkedBlockingQueue<BufferData>();
    var url = YDOC_URL + "?ls=" + WEB_SERVER_URL;

    ydoc.start();

    var ws = WsClient.builder().build();
    ws.connect(url, new TestWsListener(queue));

    var msg = queue.take();
    Assert.assertArrayEquals(new byte[] {0, 0, 1, 0}, msg.readBytes());
  }

  private static final class TestWsListener implements WsListener {

    private final BlockingQueue<BufferData> messages;

    TestWsListener(BlockingQueue<BufferData> messages) {
      this.messages = messages;
    }

    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      messages.add(buffer);
    }
  }

  private static final class WebServerWsListener implements WsListener {
    WebServerWsListener() {}
  }
}
