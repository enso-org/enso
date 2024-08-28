package org.enso.ydoc;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WebSocketRouting;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import jakarta.websocket.Endpoint;
import jakarta.websocket.EndpointConfig;
import jakarta.websocket.MessageHandler;
import jakarta.websocket.Session;
import org.enso.ydoc.jsonrpc.JsonRpcRequest;
import org.enso.ydoc.jsonrpc.JsonRpcResponse;
import org.enso.ydoc.jsonrpc.model.ContentRoot;
import org.enso.ydoc.jsonrpc.model.FilePath;
import org.enso.ydoc.jsonrpc.model.FileSystemObject;
import org.enso.ydoc.jsonrpc.model.WriteCapability;
import org.enso.ydoc.jsonrpc.model.result.FileListResult;
import org.enso.ydoc.jsonrpc.model.result.InitProtocolConnectionResult;
import org.enso.ydoc.jsonrpc.model.result.TextOpenFileResult;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class YdocTest {

  private static final int WEB_SERVER_PORT = 44556;
  private static final String YDOC_URL = "ws://localhost:1234/project/";
  private static final String WEB_SERVER_URL = "ws://127.0.0.1:" + WEB_SERVER_PORT;

  private static final Logger log = LoggerFactory.getLogger(YdocTest.class);

  private Ydoc ydoc;
  private ExecutorService webServerExecutor;
  private WebServer ls;

  private static WebServer startWebSocketServer(ExecutorService executor) {
    var routing = WebSocketRouting.builder().endpoint("/", LanguageServerConnection.class);
    var ws =
        WebServer.builder().host("localhost").port(WEB_SERVER_PORT).addRouting(routing).build();

    executor.submit(ws::start);

    return ws;
  }

  private static URI ydocUri(String doc) {
    return URI.create(YDOC_URL + doc + "?ls=" + WEB_SERVER_URL);
  }

  @Before
  public void setup() {
    webServerExecutor = Executors.newSingleThreadExecutor();
    ls = startWebSocketServer(webServerExecutor);
    ydoc = Ydoc.builder().build();
  }

  @After
  public void tearDown() throws Exception {
    ls.shutdown();
    webServerExecutor.shutdown();
    var stopped = webServerExecutor.awaitTermination(3, TimeUnit.SECONDS);
    if (!stopped) {
      var pending = webServerExecutor.shutdownNow();
      log.error("Executor pending [{}] tasks: [{}].", pending.size(), pending);
    }
    ydoc.close();
  }

  @Test
  public void initialize() throws Exception {
    var queue = new LinkedBlockingQueue<ByteBuffer>();

    ydoc.start();

    //var ws = WsClient.builder().build();
    //ws.connect(ydocUrl("index"), new DashboardConnection(queue));
    HttpClient.newHttpClient().newWebSocketBuilder().buildAsync(ydocUri("index"), new DashboardConnection(queue)).get();

    var ok1 = queue.take();
    Assert.assertTrue(ByteBufferUtil.isOk(ok1));

    var buffer = queue.take();
    var uuid = ByteBufferUtil.readUUID(buffer);
    //WsClient.builder().build().connect(ydocUrl(uuid.toString()), new DashboardConnection(queue));
    HttpClient.newHttpClient().newWebSocketBuilder().buildAsync(ydocUri(uuid.toString()), new DashboardConnection(queue)).get();

    var ok2 = queue.take();
    Assert.assertTrue(ByteBufferUtil.isOk(ok2));
  }

  private static final class ByteBufferUtil {

    private static final int UUID_BYTES = 36;
    private static final int SUFFIX_BYTES = 3;

    private static boolean isOk(ByteBuffer data) {
      //return data.readInt16() == 0;
      return data.getLong() == 0;
    }

    private static UUID readUUID(ByteBuffer data) {
      try {
        //data.skip(data.available() - UUID_BYTES - SUFFIX_BYTES);
        data.position(data.remaining() - UUID_BYTES - SUFFIX_BYTES);
        //var uuidString = data.readString(UUID_BYTES);
        var bytes = new byte[UUID_BYTES];
        data.get(bytes);
        var uuidString = new String(bytes);
        return UUID.fromString(uuidString);
      } catch (Exception e) {
        log.error("Failed to read UUID of\n{}", data);
        throw e;
      }
    }
  }

  private static final class DashboardConnection implements WebSocket.Listener {

    private static final Logger log = LoggerFactory.getLogger(DashboardConnection.class);

    private final BlockingQueue<ByteBuffer> messages;

    private DashboardConnection(BlockingQueue<ByteBuffer> messages) {
      this.messages = messages;
    }

    @Override
    public CompletionStage<?> onBinary(
        java.net.http.WebSocket webSocket, ByteBuffer buffer, boolean last) {
      log.debug("Got message\n{}", buffer);

      messages.add(buffer);

      return null;
    }

    @Override
    public CompletionStage<?> onText(WebSocket webSocket, CharSequence data, boolean last) {
      log.error("Got unexpected message [{}].", data);

      return null;
    }
  }

  private static final class LanguageServerConnection extends Endpoint {

    private static final String METHOD_INIT_PROTOCOL_CONNECTION = "session/initProtocolConnection";
    private static final String METHOD_CAPABILITY_ACQUIRE = "capability/acquire";
    private static final String METHOD_FILE_LIST = "file/list";
    private static final String METHOD_TEXT_OPEN_FILE = "text/openFile";

    private static final UUID PROJECT_ROOT_ID = new UUID(0, 1);

    private static final Logger log = LoggerFactory.getLogger(LanguageServerConnection.class);

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private LanguageServerConnection() {}

    @Override
    public void onOpen(Session session, EndpointConfig config) {
      session.addMessageHandler((MessageHandler.Whole<String>) message -> onMessage(session, message));
    }

    private void onMessage(Session session, String text) {
      log.debug("Got message [{}].", text);

      try {
        var request = objectMapper.readValue(text, JsonRpcRequest.class);

        JsonRpcResponse jsonRpcResponse = null;

        switch (request.method()) {
          case METHOD_INIT_PROTOCOL_CONNECTION -> {
            var contentRoots =
                List.of(
                    new ContentRoot("Project", PROJECT_ROOT_ID),
                    new ContentRoot("Home", new UUID(0, 2)),
                    new ContentRoot("FileSystemRoot", new UUID(0, 3), "/"));
            var initProtocolConnectionResult =
                new InitProtocolConnectionResult("0.0.0-dev", "0.0.0-dev", contentRoots);
            jsonRpcResponse = new JsonRpcResponse(request.id(), initProtocolConnectionResult);
          }
          case METHOD_CAPABILITY_ACQUIRE -> jsonRpcResponse = JsonRpcResponse.ok(request.id());
          case METHOD_FILE_LIST -> {
            var paths =
                List.of(
                    FileSystemObject.file(
                        "Main.enso", new FilePath(PROJECT_ROOT_ID, List.of("src"))));
            var fileListResult = new FileListResult(paths);
            jsonRpcResponse = new JsonRpcResponse(request.id(), fileListResult);
          }
          case METHOD_TEXT_OPEN_FILE -> {
            var options =
                new WriteCapability.Options(
                    new FilePath(PROJECT_ROOT_ID, List.of("src", "Main.enso")));
            var writeCapability = new WriteCapability("text/canEdit", options);
            var textOpenFileResult =
                new TextOpenFileResult(
                    writeCapability,
                    "main =",
                    "e5aeae8609bd90f94941d4227e6ec1e0f069d3318fb7bd93ffe4d391");
            jsonRpcResponse = new JsonRpcResponse(request.id(), textOpenFileResult);
          }
        }

        if (jsonRpcResponse != null) {
          var response = objectMapper.writeValueAsString(jsonRpcResponse);

          log.debug("Sending [{}].", response);
          session.getBasicRemote().sendText(response, true);
        } else {
          log.error("Unknown request.");
        }
      } catch (JsonProcessingException e) {
        log.error("Failed to parse JSON.", e);
        Assert.fail(e.getMessage());
      } catch (IOException e) {
        log.error("LS failed send reply.", e);
        Assert.fail(e.getMessage());
      }
    }
  }
}
