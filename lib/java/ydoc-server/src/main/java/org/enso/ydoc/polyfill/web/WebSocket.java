package org.enso.ydoc.polyfill.web;

import io.helidon.common.buffers.BufferData;
import io.helidon.http.Headers;
import io.helidon.http.HttpPrologue;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import org.enso.ydoc.polyfill.Arguments;
import org.enso.ydoc.polyfill.PolyfillBase;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.ByteSequence;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements the WebSocket and WebSocketServer interfaces of the <a
 * href="https://www.npmjs.com/package/ws">ws</a> NPM package.
 */
final class WebSocket extends PolyfillBase implements ProxyExecutable {

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
    super(WEBSOCKET_JS);
    this.executor = executor;
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
            new JavaHttpWebSocketConnection(
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

        var wsBuilder = HttpClient.newHttpClient().newWebSocketBuilder();

        if (protocols != null) {
          if (protocols.length > 0) {
            wsBuilder.subprotocols(protocols[0], Arrays.copyOfRange(protocols, 1, protocols.length));
          }
        }

        try {
          wsBuilder.buildAsync(uri, connection).get();
        } catch (InterruptedException | ExecutionException e) {
          throw new RuntimeException(e);
        }

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

        yield new HelidonWebSocketConnection(
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
                              () -> handleConnect.execute().as(HelidonWebSocketConnection.class));

                      HelidonWebSocketConnection connection;
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

        yield connection.getSession().send(ByteBuffer.wrap(byteArray), true);
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

        yield connection.getSession().ping(ByteBuffer.wrap(byteArray));
      }

      case WEB_SOCKET_PONG -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var byteSequence = arguments[2].as(ByteSequence.class);
        var byteOffset = arguments[3].asInt();
        var byteLength = arguments[4].asInt();

        var byteArray = byteSequence.subSequence(byteOffset, byteOffset + byteLength).toByteArray();

        yield connection.getSession().pong(ByteBuffer.wrap(byteArray));
      }

      default -> throw new IllegalStateException(command);
    };
  }

  private interface WebSocketSession {

    WebSocketSession send(CharSequence text, Boolean last);

    WebSocketSession send(ByteBuffer buffer, Boolean last);

    WebSocketSession ping(ByteBuffer buffer);

    WebSocketSession pong(ByteBuffer buffer);

    WebSocketSession close(int code, String reason);

    WebSocketSession terminate();
  }

  private static final class HelidonWebSocketSession implements WebSocketSession {

    private WsSession session;

    private HelidonWebSocketSession(WsSession session) {
      this.session = session;
    }

    @Override
    public WebSocketSession send(CharSequence text, Boolean last) {
      this.session = session.send(text.toString(), last);

      return this;
    }

    @Override
    public WebSocketSession send(ByteBuffer buffer, Boolean last) {
      var bytesArray = new byte[buffer.remaining()];
      buffer.get(bytesArray);
      this.session = session.send(BufferData.create(bytesArray), last);

      return this;
    }

    @Override
    public WebSocketSession ping(ByteBuffer buffer) {
      var bytesArray = new byte[buffer.remaining()];
      buffer.get(bytesArray);
      this.session = session.ping(BufferData.create(bytesArray));

      return this;
    }

    @Override
    public WebSocketSession pong(ByteBuffer buffer) {
      var bytesArray = new byte[buffer.remaining()];
      buffer.get(bytesArray);
      this.session = session.pong(BufferData.create(bytesArray));

      return this;
    }

    @Override
    public WebSocketSession close(int code, String reason) {
      this.session = session.close(code, reason);

      return this;
    }

    @Override
    public WebSocketSession terminate() {
      this.session = session.terminate();

      return this;
    }
  }

  private static final class JavaHttpWebSocketSession implements WebSocketSession {

    private java.net.http.WebSocket webSocket;

    private JavaHttpWebSocketSession(java.net.http.WebSocket webSocket) {
      this.webSocket = webSocket;
    }

    @Override
    public WebSocketSession send(CharSequence text, Boolean last) {
      try {
        this.webSocket = webSocket.sendText(text, last).get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }

      return this;
    }

    @Override
    public WebSocketSession send(ByteBuffer buffer, Boolean last) {
      try {
        this.webSocket = webSocket.sendBinary(buffer, last).get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }

      return this;
    }

    @Override
    public WebSocketSession ping(ByteBuffer buffer) {
      try {
        this.webSocket = webSocket.sendPing(buffer).get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }

      return this;
    }

    @Override
    public WebSocketSession pong(ByteBuffer buffer) {
      try {
        this.webSocket = webSocket.sendPong(buffer).get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }
      return this;
    }

    @Override
    public WebSocketSession close(int code, String reason) {
      try {
        this.webSocket = webSocket.sendClose(code, reason).get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }

      return this;
    }

    @Override
    public WebSocketSession terminate() {
      try {
        this.webSocket = webSocket.sendClose(java.net.http.WebSocket.NORMAL_CLOSURE, "").get();
      } catch (InterruptedException | ExecutionException e) {
        throw new RuntimeException(e);
      }

      return this;
    }
  }

  private interface WebSocketConnection {

    WebSocketSession getSession();
  }

  private static final class HelidonWebSocketConnection implements WsListener, WebSocketConnection {

    private final ExecutorService executor;

    private final Value handleOpen;
    private final Value handleClose;
    private final Value handleError;
    private final Value handleMessage;
    private final Value handlePing;
    private final Value handlePong;
    private final Value handleUpgrade;

    private WebSocketSession session;

    private HelidonWebSocketConnection(
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

    @Override
    public WebSocketSession getSession() {
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

      this.session = new HelidonWebSocketSession(session);

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

  private static final class JavaHttpWebSocketConnection implements java.net.http.WebSocket.Listener, WebSocketConnection {

    private final ExecutorService executor;

    private final Value handleOpen;
    private final Value handleClose;
    private final Value handleError;
    private final Value handleMessage;
    private final Value handlePing;
    private final Value handlePong;

    private WebSocketSession session;

    private JavaHttpWebSocketConnection(
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
    }

    @Override
    public WebSocketSession getSession() {
      return session;
    }

    /*
     * Callbacks
     */
    @Override
    public CompletionStage<?> onBinary(java.net.http.WebSocket webSocket, ByteBuffer buffer, boolean last) {
      log.debug("onMessage [{}]", buffer);

      // Passing byte sequence to JS requires `HostAccess.allowBufferAccess()`
      var bytesArray = new byte[buffer.remaining()];
      buffer.get(bytesArray);
      var bytes = ByteSequence.create(bytesArray);
      executor.execute(() -> handleMessage.executeVoid(bytes));
      webSocket.request(1);

      return null;
    }

    @Override
    public CompletionStage<?> onText(java.net.http.WebSocket webSocket, CharSequence data, boolean last) {
      log.debug("onMessage [{}]", data);

      executor.execute(() -> handleMessage.executeVoid(data.toString()));
      webSocket.request(1);

      return null;
    }

    @Override
    public CompletionStage<?> onPing(java.net.http.WebSocket webSocket, ByteBuffer buffer) {
      log.debug("onPing [{}]", buffer);

      var bytesArray = new byte[buffer.remaining()];
      buffer.get(bytesArray);
      var bytes = ByteSequence.create(bytesArray);
      executor.execute(() -> handlePing.executeVoid(bytes));
      webSocket.request(1);

      return null;
    }

    @Override
    public CompletionStage<?> onPong(java.net.http.WebSocket webSocket, ByteBuffer buffer) {
      log.debug("onPong [{}]", buffer);

      var bytesArray = new byte[buffer.remaining()];
      buffer.get(bytesArray);
      var bytes = ByteSequence.create(bytesArray);
      executor.execute(() -> handlePong.executeVoid(bytes));
      webSocket.request(1);

      return null;
    }

    @Override
    public void onOpen(java.net.http.WebSocket webSocket) {
      log.debug("onOpen");

      this.session = new JavaHttpWebSocketSession(webSocket);
      executor.execute(() -> handleOpen.executeVoid());
      webSocket.request(1);
    }

    @Override
    public CompletionStage<?> onClose(java.net.http.WebSocket webSocket, int status, String reason) {
      log.debug("onClose [{}] [{}]", status, reason);

      executor.execute(() -> handleClose.executeVoid(status, reason));
      this.session = null;

      return null;
    }

    @Override
    public void onError(java.net.http.WebSocket webSocket, Throwable t) {
      log.error("onError ", t);

      executor.execute(() -> handleError.executeVoid(t.getMessage()));
    }
  }
}
