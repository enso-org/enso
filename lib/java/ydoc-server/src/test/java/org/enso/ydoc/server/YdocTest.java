package org.enso.ydoc.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.helidon.common.buffers.BufferData;
import io.helidon.http.Status;
import io.helidon.webclient.api.HttpClientResponse;
import io.helidon.webclient.api.WebClient;
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
import org.enso.ydoc.server.jsonrpc.JsonRpcRequest;
import org.enso.ydoc.server.jsonrpc.JsonRpcResponse;
import org.enso.ydoc.server.jsonrpc.model.ContentRoot;
import org.enso.ydoc.server.jsonrpc.model.FilePath;
import org.enso.ydoc.server.jsonrpc.model.FileSystemObject;
import org.enso.ydoc.server.jsonrpc.model.WriteCapability;
import org.enso.ydoc.server.jsonrpc.model.result.FileListResult;
import org.enso.ydoc.server.jsonrpc.model.result.InitProtocolConnectionResult;
import org.enso.ydoc.server.jsonrpc.model.result.TextOpenFileResult;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class YdocTest {

  private static final int WEB_SERVER_PORT = 44556;
  private static final String YDOC_URL = "ws://localhost:1234/project/";
  private static final String HEALTHCHECK_URL = "http://localhost:1234/_health";
  private static final String WEB_SERVER_URL = "ws://127.0.0.1:" + WEB_SERVER_PORT;

  private static final Logger log = LoggerFactory.getLogger(YdocTest.class);

  private Ydoc ydoc;
  private ExecutorService webServerExecutor;
  private WebServer ls;

  private static WebServer startWebSocketServer(ExecutorService executor) {
    var routing = WsRouting.builder().endpoint("/", new LanguageServerConnection());
    var ws =
        WebServer.builder().host("localhost").port(WEB_SERVER_PORT).addRouting(routing).build();

    executor.submit(ws::start);

    return ws;
  }

  private static String ydocUrl(String doc) {
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
    var queue = new LinkedBlockingQueue<BufferData>();

    ydoc.start();

    var ws = WsClient.builder().build();
    ws.connect(ydocUrl("index"), new DashboardConnection(queue));

    var ok1 = queue.take();
    Assert.assertTrue(ok1.debugDataHex(), BufferDataUtil.isOk(ok1));

    var buffer = queue.take();
    var uuid = BufferDataUtil.readUUID(buffer);
    WsClient.builder().build().connect(ydocUrl(uuid.toString()), new DashboardConnection(queue));

    var ok2 = queue.take();
    Assert.assertTrue(ok2.debugDataHex(), BufferDataUtil.isOk(ok2));

    WebClient http = WebClient.create();
    HttpClientResponse healthcheckResponse = http.get(HEALTHCHECK_URL).request();
    Assert.assertEquals(Status.OK_200, healthcheckResponse.status());
  }

  private static final class BufferDataUtil {

    private static final int UUID_BYTES = 36;
    private static final int SUFFIX_BYTES = 3;

    private static boolean isOk(BufferData data) {
      return data.readInt16() == 0;
    }

    private static UUID readUUID(BufferData data) {
      try {
        data.skip(data.available() - UUID_BYTES - SUFFIX_BYTES);
        var uuidString = data.readString(UUID_BYTES);
        return UUID.fromString(uuidString);
      } catch (Exception e) {
        log.error("Failed to read UUID of\n{}", data.debugDataHex());
        throw e;
      }
    }
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

  private static final class LanguageServerConnection implements WsListener {

    private static final String METHOD_INIT_PROTOCOL_CONNECTION = "session/initProtocolConnection";
    private static final String METHOD_CAPABILITY_ACQUIRE = "capability/acquire";
    private static final String METHOD_FILE_LIST = "file/list";
    private static final String METHOD_TEXT_OPEN_FILE = "text/openFile";

    private static final UUID PROJECT_ROOT_ID = new UUID(0, 1);

    private static final Logger log = LoggerFactory.getLogger(LanguageServerConnection.class);

    private static final ObjectMapper objectMapper = new ObjectMapper();

    private LanguageServerConnection() {}

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
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
          session.send(response, true);
        } else {
          log.error("Unknown request.");
        }
      } catch (JsonProcessingException e) {
        log.error("Failed to parse JSON.", e);
        Assert.fail(e.getMessage());
      }
    }
  }
}
