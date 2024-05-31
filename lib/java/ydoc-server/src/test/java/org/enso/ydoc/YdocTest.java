package org.enso.ydoc;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.common.buffers.BufferData;
import io.helidon.webclient.websocket.WsClient;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.jsonrpc.Request;
import org.enso.ydoc.jsonrpc.Response;
import org.enso.ydoc.jsonrpc.model.ContentRoot;
import org.enso.ydoc.jsonrpc.model.FilePath;
import org.enso.ydoc.jsonrpc.model.FileSystemObject;
import org.enso.ydoc.jsonrpc.model.WriteCapability;
import org.enso.ydoc.jsonrpc.result.FileListResult;
import org.enso.ydoc.jsonrpc.result.InitProtocolConnectionResult;
import org.enso.ydoc.jsonrpc.result.TextOpenFileResult;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class YdocTest {

  private static final int WEB_SERVER_PORT = 44556;
  private static final String YDOC_URL = "ws://localhost:1234/project/";
  private static final String WEB_SERVER_URL = "ws://127.0.0.1:" + WEB_SERVER_PORT;

  private static final UUID PROJECT_ID = new UUID(0, 1);

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

    ydoc.start();

    var ws = WsClient.builder().build();
    ws.connect(ydocUrl("index"), new TestWsListener(queue));

    var ok1 = queue.take();
    Assert.assertArrayEquals(new byte[] {0, 0, 1, 0}, ok1.readBytes());

    var buffer = queue.take();
    buffer.skip(32);
    var uuidString = buffer.readString(36);
    System.out.println("UUID_STRING='" + uuidString + "'");
    var uuid = UUID.fromString(uuidString);
    System.out.println("UUID='" + uuid + "'");

    WsClient.builder().build().connect(ydocUrl(uuid), new TestWsListener(queue));

    var ok2 = queue.take();
    Assert.assertArrayEquals(new byte[] {0, 0, 1, 0}, ok2.readBytes());

    Thread.sleep(10000);
  }

  private static final class TestWsListener implements WsListener {

    private final BlockingQueue<BufferData> messages;

    TestWsListener(BlockingQueue<BufferData> messages) {
      this.messages = messages;
    }

    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      System.out.println("!!!!!!!!!!C onMessage\n" + buffer.debugDataHex(true));

      messages.add(buffer);
    }

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      System.out.println("!!!!!!!!!!C onMessage" + text);
    }
  }

  private static final class WebServerWsListener implements WsListener {

    private static final String METHOD_INIT_PROTOCOL_CONNECTION = "session/initProtocolConnection";
    private static final String METHOD_CAPABILITY_ACQUIRE = "capability/acquire";
    private static final String METHOD_FILE_LIST = "file/list";
    private static final String METHOD_TEXT_OPEN_FILE = "text/openFile";

    private static final ObjectMapper objectMapper = new ObjectMapper();

    WebServerWsListener() {}

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      System.out.println("!!!!!!!S onMessage " + text);
      try {
        var request = objectMapper.readValue(text, Request.class);

        Response jsonRpcResponse = null;

        switch (request.method()) {
          case METHOD_INIT_PROTOCOL_CONNECTION -> {
            var contentRoots =
                List.of(
                    new ContentRoot("Project", PROJECT_ID),
                    new ContentRoot("Home", new UUID(0, 2)),
                    new ContentRoot("FileSystemRoot", new UUID(0, 3), "/"));
            var initProtocolConnectionResult =
                new InitProtocolConnectionResult("0.0.0-dev", "0.0.0-dev", contentRoots);
            jsonRpcResponse = new Response(request.id(), initProtocolConnectionResult);
          }
          case METHOD_CAPABILITY_ACQUIRE -> jsonRpcResponse = Response.ok(request.id());
          case METHOD_FILE_LIST -> {
            var paths =
                List.of(
                    FileSystemObject.file("Main.enso", new FilePath(PROJECT_ID, List.of("src"))));
            var fileListResult = new FileListResult(paths);
            jsonRpcResponse = new Response(request.id(), fileListResult);
          }
          case METHOD_TEXT_OPEN_FILE -> {
            var options =
                new WriteCapability.Options(new FilePath(PROJECT_ID, List.of("src", "Main.enso")));
            var writeCapability = new WriteCapability("text/canEdit", options);
            var textOpenFileResult =
                new TextOpenFileResult(
                    writeCapability, TextOpenFileResult.CONTENT, TextOpenFileResult.VERSION);
            jsonRpcResponse = new Response(request.id(), textOpenFileResult);
          }
        }

        if (jsonRpcResponse != null) {
          var response = objectMapper.writeValueAsString(jsonRpcResponse);
          System.out.println("SENDING " + response);
          session.send(response, true);
        } else {
          System.out.println("UNKNOWN REQUEST");
        }
      } catch (JsonProcessingException e) {
        e.printStackTrace();
        throw new RuntimeException(e);
      }
    }
  }
}
