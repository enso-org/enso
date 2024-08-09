package org.enso.ydoc.polyfill.web;

import io.helidon.common.buffers.BufferData;
import io.helidon.http.Headers;
import io.helidon.http.HttpPrologue;
import io.helidon.webclient.websocket.WsClient;
import io.helidon.webclient.websocket.WsClientProtocolConfig;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import org.enso.ydoc.Polyfill;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.ByteSequence;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements the WebSocket and WebSocketServer interfaces of the <a
 * href="https://www.npmjs.com/package/ws">ws</a> NPM package.
 */
final class WebSocket implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(WebSocket.class);

  private static final String NEW_WEB_SOCKET = "new-web-socket";
  private static final String NEW_WEB_SOCKET_CONNECTION = "new-web-socket-connection";
  private static final String WEB_SOCKET_SEND_TEXT = "web-socket-send-text";
  private static final String WEB_SOCKET_SEND_BINARY = "web-socket-send-binary";
  private static final String WEB_SOCKET_CLOSE = "web-socket-close";
  private static final String WEB_SOCKET_PING = "web-socket-ping";
  private static final String WEB_SOCKET_PONG = "web-socket-pong";
  private static final String WEB_SOCKET_TERMINATE = "web-socket-terminate";
  private static final String NEW_WEB_SOCKET_SERVER = "new-web-socket-server";
  private static final String WEB_SOCKET_SERVER_START = "web-socket-server-start";

  private static final String WEBSOCKET_JS = "websocket.js";

  private final ExecutorService executor;

  WebSocket(ExecutorService executor) {
    this.executor = executor;
  }

  @Override
  public void initialize(Context ctx) {
    Source jsSource = Source.newBuilder("js", getClass().getResource(WEBSOCKET_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case NEW_WEB_SOCKET -> {
        var urlString = arguments[1].asString();
        var protocols = arguments[2].as(String[].class);
        var handleOpen = arguments[3];
        var handleClose = arguments[4];
        var handleError = arguments[5];
        var handleMessage = arguments[6];
        var handlePing = arguments[7];
        var handlePong = arguments[8];
        var handleUpgrade = arguments[9];
        var connection =
            new WebSocketConnection(
                executor,
                handleOpen,
                handleClose,
                handleError,
                handleMessage,
                handlePing,
                handlePong,
                handleUpgrade);

        URI uri;
        try {
          uri = new URI(urlString);
        } catch (URISyntaxException ex) {
          throw new IllegalStateException("Illegal URL", ex);
        }

        var protocolConfig = WsClientProtocolConfig.builder();
        if (protocols != null) {
          protocolConfig.subProtocols(Arrays.asList(protocols));
        }

        var wsClient = WsClient.builder().protocolConfig(protocolConfig.build()).build();
        wsClient.connect(uri, connection);

        yield connection;
      }

      case NEW_WEB_SOCKET_CONNECTION -> {
        var handleOpen = arguments[1];
        var handleClose = arguments[2];
        var handleError = arguments[3];
        var handleMessage = arguments[4];
        var handlePing = arguments[5];
        var handlePong = arguments[6];
        var handleUpgrade = arguments[7];

        yield new WebSocketConnection(
            executor,
            handleOpen,
            handleClose,
            handleError,
            handleMessage,
            handlePing,
            handlePong,
            handleUpgrade);
      }

      case NEW_WEB_SOCKET_SERVER -> {
        var host = arguments[1].asString();
        var port = arguments[2].asInt();
        var handleConnect = arguments[3];

        var routing =
            WsRouting.builder()
                .endpoint(
                    "*",
                    () -> {
                      var connectionFuture =
                          executor.submit(
                              () -> handleConnect.execute().as(WebSocketConnection.class));

                      WebSocketConnection connection;
                      try {
                        connection = connectionFuture.get();
                      } catch (InterruptedException | ExecutionException e) {
                        log.error("Connection error", e);
                        throw new RuntimeException(e);
                      }

                      return connection;
                    });

        yield WebServer.builder().host(host).port(port).addRouting(routing).build();
      }

      case WEB_SOCKET_SERVER_START -> {
        var webServer = arguments[1].as(WebServer.class);

        yield webServer.start();
      }

      case WEB_SOCKET_SEND_TEXT -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var data = arguments[2].asString();

        yield connection.getSession().send(data, true);
      }

      case WEB_SOCKET_SEND_BINARY -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var byteSequence = arguments[2].as(ByteSequence.class);
        var byteOffset = arguments[3].asInt();
        var byteLength = arguments[4].asInt();

        var byteArray = byteSequence.subSequence(byteOffset, byteOffset + byteLength).toByteArray();

        yield connection.getSession().send(BufferData.create(byteArray), true);
      }

      case WEB_SOCKET_TERMINATE -> {
        var connection = arguments[1].as(WebSocketConnection.class);

        var session = connection.getSession();
        if (session != null) {
          session.terminate();
        }

        yield null;
      }

      case WEB_SOCKET_CLOSE -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var code = arguments[2].asInt();
        var reasonArgument = arguments[3].asString();

        var session = connection.getSession();
        if (session != null) {
          var reason = reasonArgument == null ? "Close" : reasonArgument;
          session.close(code, reason);
        }

        yield null;
      }

      case WEB_SOCKET_PING -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var byteSequence = arguments[2].as(ByteSequence.class);
        var byteOffset = arguments[3].asInt();
        var byteLength = arguments[4].asInt();

        var byteArray = byteSequence.subSequence(byteOffset, byteOffset + byteLength).toByteArray();

        yield connection.getSession().ping(BufferData.create(byteArray));
      }

      case WEB_SOCKET_PONG -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var byteSequence = arguments[2].as(ByteSequence.class);
        var byteOffset = arguments[3].asInt();
        var byteLength = arguments[4].asInt();

        var byteArray = byteSequence.subSequence(byteOffset, byteOffset + byteLength).toByteArray();

        yield connection.getSession().pong(BufferData.create(byteArray));
      }

      default -> throw new IllegalStateException(command);
    };
  }

  private static final class WebSocketConnection implements WsListener {

    private final ExecutorService executor;

    private final Value handleOpen;
    private final Value handleClose;
    private final Value handleError;
    private final Value handleMessage;
    private final Value handlePing;
    private final Value handlePong;
    private final Value handleUpgrade;

    private WsSession session;

    private WebSocketConnection(
        ExecutorService executor,
        Value handleOpen,
        Value handleClose,
        Value handleError,
        Value handleMessage,
        Value handlePing,
        Value handlePong,
        Value handleUpgrade) {
      this.executor = executor;
      this.handleOpen = handleOpen;
      this.handleClose = handleClose;
      this.handleError = handleError;
      this.handleMessage = handleMessage;
      this.handlePing = handlePing;
      this.handlePong = handlePong;
      this.handleUpgrade = handleUpgrade;
    }

    public WsSession getSession() {
      return session;
    }

    /*
     * Callbacks
     */
    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      log.debug("onMessage\n{}", buffer.debugDataHex(true));

      // Passing byte sequence to JS requires `HostAccess.allowBufferAccess()`
      var bytes = ByteSequence.create(buffer.readBytes());
      executor.execute(() -> handleMessage.executeVoid(bytes));
    }

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      log.debug("onMessage [{}]", text);

      executor.execute(() -> handleMessage.executeVoid(text));
    }

    @Override
    public void onPing(WsSession session, BufferData buffer) {
      log.debug("onPing [{}]", buffer);

      var bytes = ByteSequence.create(buffer.readBytes());
      executor.execute(() -> handlePing.executeVoid(bytes));
    }

    @Override
    public void onPong(WsSession session, BufferData buffer) {
      log.debug("onPong [{}]", buffer);

      var bytes = ByteSequence.create(buffer.readBytes());
      executor.execute(() -> handlePong.executeVoid(bytes));
    }

    @Override
    public void onOpen(WsSession session) {
      log.debug("onOpen");

      this.session = session;

      executor.execute(() -> handleOpen.executeVoid());
    }

    @Override
    public void onClose(WsSession session, int status, String reason) {
      log.debug("onClose [{}] [{}]", status, reason);

      executor.execute(() -> handleClose.executeVoid(status, reason));
      this.session = null;
    }

    @Override
    public void onError(WsSession session, Throwable t) {
      log.error("onError ", t);

      executor.execute(() -> handleError.executeVoid(t.getMessage()));
    }

    @Override
    public Optional<Headers> onHttpUpgrade(HttpPrologue prologue, Headers headers) {
      log.debug("onHttpUpgrade [{}]", prologue);

      var url = new URL(prologue);
      executor.execute(() -> handleUpgrade.executeVoid(url));

      return Optional.empty();
    }
  }
}
