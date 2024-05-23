package org.enso.ydoc;

import io.helidon.common.buffers.BufferData;
import io.helidon.webclient.websocket.WsClient;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;

import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class YdocTest {

  private static final int WEB_SERVER_PORT = 44556;
  private static final String YDOC_URL = "ws://localhost:1234/project/";
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

  private static String ydocUrl(Object doc) {
    return YDOC_URL + doc + "?ls=" + WEB_SERVER_URL;
  }

  @Before
  public void setup() {
    webServerExecutor = Executors.newSingleThreadExecutor();
    ls = startWebSocketServer(webServerExecutor);
    ydoc = Ydoc.builder().build();
  }

  @After
  public void tearDown() throws Exception {
    ls.stop();
    webServerExecutor.shutdownNow();
    webServerExecutor.awaitTermination(3, TimeUnit.SECONDS);
    ydoc.close();
  }

  @Test
  public void connect() throws Exception {
    var queue = new LinkedBlockingQueue<BufferData>();
    var projectId = new UUID(0, 1);

    ydoc.start();

    var ws = WsClient.builder().build();
    ws.connect(ydocUrl("index"), new TestWsListener(queue));

    var msg = queue.take();
    Assert.assertArrayEquals(new byte[] {0, 0, 1, 0}, msg.readBytes());

    //WsClient.builder().build().connect(ydocUrl(projectId), new TestWsListener(queue));
    Thread.sleep(10000);
  }

  private static final class TestWsListener implements WsListener {

    private final BlockingQueue<BufferData> messages;

    TestWsListener(BlockingQueue<BufferData> messages) {
      this.messages = messages;
    }

    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      System.out.println(buffer.debugDataHex(true));
      messages.add(buffer);
    }

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      System.out.println("!!!!!!!!!!C onMessage" + text);
    }
  }

  private static final class WebServerWsListener implements WsListener {

    private static final String INIT_PROTOCOL_CONNECTION_REQUEST = "session/initProtocolConnection";
    private static final String INIT_PROTOCOL_CONNECTION_RESPONSE =
        """
{"jsonrpc":"2.0","id":"0","result":{"ensoVersion":"0.0.0-dev","currentEdition":"0.0.0-dev","contentRoots":[{"type":"Project","id":"c0223c1f-18c5-45bb-a175-70b933caa357"},{"type":"Home","id":"638ae233-03a5-4d15-bd4b-9ef159180917"},{"type":"FileSystemRoot","id":"6173bd12-5570-46fd-8423-902c161b9f8b","path":"/"}]}}""";

    WebServerWsListener() {}

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      System.out.println("!!!!!!!S onMessage " + text);
      if (text.contains(INIT_PROTOCOL_CONNECTION_REQUEST)) {
        session.send(INIT_PROTOCOL_CONNECTION_RESPONSE, true);
      }
    }
  }

}
