package org.enso.ydoc.polyfill;

import io.helidon.common.buffers.BufferData;
import io.helidon.webclient.websocket.WsClient;
import io.helidon.webclient.websocket.WsClientProtocolConfig;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.websocket.WsRouting;
import io.helidon.websocket.WsListener;
import io.helidon.websocket.WsSession;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

final class WebSocket implements ProxyExecutable, Polyfill {

  private static final String NEW_WEB_SOCKET = "new-web-socket";
  private static final String NEW_WEB_SOCKET_CONECTION = "new-web-socket-connection";
  private static final String WEB_SOCKET_SEND_TEXT = "web-socket-send-text";
  private static final String WEB_SOCKET_SEND_BINARY = "web-socket-send-binary";
  private static final String WEB_SOCKET_CLOSE = "web-socket-close";
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
    Source webSocketJs =
        Source.newBuilder("js", WebSocket.class.getResource(WEBSOCKET_JS)).buildLiteral();

    ctx.eval(webSocketJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();
    System.err.println(command + " " + Arrays.toString(arguments));

    return switch (command) {
      case NEW_WEB_SOCKET -> {
        var urlString = arguments[1].asString();
        var protocols = arguments[2].as(String[].class);
        var handleOpen = arguments[3];
        var handleClose = arguments[4];
        var handleError = arguments[5];
        var handleMessage = arguments[6];
        var connection =
            new WebSocketConnection(
                executor, handleOpen, handleClose, handleError, handleMessage);

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

      case NEW_WEB_SOCKET_CONECTION -> {
        var handleOpen = arguments[1];
        var handleClose = arguments[2];
        var handleError = arguments[3];
        var handleMessage = arguments[4];
        var connection =
            new WebSocketConnection(
                executor, handleOpen, handleClose, handleError, handleMessage);

        yield connection;
      }

      case NEW_WEB_SOCKET_SERVER -> {
        var host = arguments[1].asString();
        var port = arguments[2].asInt();
        var handleConnect = arguments[3];

        var routing =
            WsRouting.builder()
                .endpoint(
                    "/",
                    () -> {
                      var connectionFuture =
                          executor.submit(
                              () -> handleConnect.execute().as(WebSocketConnection.class));

                      WebSocketConnection connection;
                      try {
                        connection = connectionFuture.get();
                      } catch (InterruptedException | ExecutionException e) {
                        System.err.println("WebSocketServer connection error: " + e);
                        throw new RuntimeException(e);
                      }

                      return connection;
                    });
        var webServer = WebServer.builder().host(host).port(port).addRouting(routing).build();

        yield webServer;
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
        var data = arguments[2].as(int[].class);

        // Convert unsigned Uint8Array to byte[]
        var bytes = new byte[data.length];
        for (int i = 0; i < data.length; i++) {
          bytes[i] = (byte) data[i];
        }
        var bufferData = BufferData.create(bytes);

        yield connection.getSession().send(bufferData, true);
      }

      case WEB_SOCKET_TERMINATE -> {
        var connection = arguments[1].as(WebSocketConnection.class);

        yield connection.getSession().terminate();
      }

      case WEB_SOCKET_CLOSE -> {
        var connection = arguments[1].as(WebSocketConnection.class);
        var code = arguments[2].asInt();
        var reasonArgument = arguments[3];
        var reason = reasonArgument == null ? "Close" : reasonArgument.asString();

        yield connection.getSession().close(code, reason);
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

    private WsSession session;

    private WebSocketConnection(
        ExecutorService executor,
        Value handleOpen,
        Value handleClose,
        Value handleError,
        Value handleMessage) {
      this.executor = executor;
      this.handleOpen = handleOpen;
      this.handleClose = handleClose;
      this.handleError = handleError;
      this.handleMessage = handleMessage;
    }

    public WsSession getSession() {
      return session;
    }

    /*
     * Callbacks
     */
    @Override
    public void onMessage(WsSession session, BufferData buffer, boolean last) {
      System.err.println("WebSocketListener.onMessageBinary\n" + buffer.debugDataHex(true));

      // Passing byte array to JS requires `HostAccess.allowArrayAccess()`
      Object data = buffer.readBytes();
      executor.execute(() -> handleMessage.executeVoid(data));
    }

    @Override
    public void onMessage(WsSession session, String text, boolean last) {
      System.err.println("WebSocketListener.onMessage " + text);

      executor.execute(() -> handleMessage.executeVoid(text));
    }

    @Override
    public void onPing(WsSession session, BufferData buffer) {
      System.err.println("WebSocketListener.onPing " + buffer);
    }

    @Override
    public void onPong(WsSession session, BufferData buffer) {
      System.err.println("WebSocketListener.onPong " + buffer);
    }

    @Override
    public void onOpen(WsSession session) {
      System.err.println("WebSocketListener.onOpen");

      this.session = session;
      executor.execute(() -> handleOpen.executeVoid());
    }

    @Override
    public void onClose(WsSession session, int status, String reason) {
      System.err.println("WebSocketListener.onClose " + status + " " + reason);

      executor.execute(() -> handleClose.executeVoid(status, reason));
      this.session = null;
    }

    @Override
    public void onError(WsSession session, Throwable t) {
      System.err.println("WebSocketListener.onError " + t);

      executor.execute(() -> handleError.executeVoid(t.getMessage()));
    }
  }
}
