package org.enso.logging.service.logback;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.joran.JoranConfigurator;
import ch.qos.logback.core.joran.spi.JoranException;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import javax.net.ServerSocketFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A direct copy of ch.qos.logback.classic.net.SimpleSocketServer. A simple {@link
 * SocketLoggingNode} based server.
 *
 * @author Ceki G&uuml;lc&uuml;
 * @author S&eacute;bastien Pennec
 * @since 0.8.4
 */
public class SocketServer extends Thread {

  Logger logger = LoggerFactory.getLogger(SocketServer.class);

  private final int port;
  private final LoggerContext lc;
  private boolean closed = false;
  private ServerSocket serverSocket;
  private List<SocketLoggingNode> socketNodeList = new ArrayList<>();

  // used for testing purposes
  private CountDownLatch latch;

  public SocketServer(LoggerContext lc, int port) {
    this.lc = lc;
    this.port = port;
  }

  public void run() {

    final String oldThreadName = Thread.currentThread().getName();

    try {

      final String newThreadName = getServerThreadName();
      Thread.currentThread().setName(newThreadName);

      logger.debug("Listening on port " + port);
      serverSocket = getServerSocketFactory().createServerSocket(port);
      while (!closed) {
        logger.debug("Waiting to accept a new client.");
        signalAlmostReadiness();
        Socket socket = serverSocket.accept();
        logger.debug("Connected to client at " + socket.getInetAddress());
        logger.debug("Starting new socket node.");
        SocketLoggingNode newSocketNode = new SocketLoggingNode(this, socket, lc);
        synchronized (socketNodeList) {
          socketNodeList.add(newSocketNode);
        }
        final String clientThreadName = getClientThreadName(socket);
        new Thread(newSocketNode, clientThreadName).start();
      }
    } catch (Exception e) {
      if (closed) {
        logger.warn("Exception in run method for a closed server", e);
      } else {
        logger.error("Unexpected failure in run method", e);
      }
    } finally {
      Thread.currentThread().setName(oldThreadName);
    }
  }

  /** Returns the name given to the server thread. */
  protected String getServerThreadName() {
    return String.format("Logback %s (port %d)", getClass().getSimpleName(), port);
  }

  /** Returns a name to identify each client thread. */
  protected String getClientThreadName(Socket socket) {
    return String.format("Logback SocketNode (client: %s)", socket.getRemoteSocketAddress());
  }

  /**
   * Gets the platform default {@link ServerSocketFactory}.
   *
   * <p>Subclasses may override to provide a custom server socket factory.
   */
  protected ServerSocketFactory getServerSocketFactory() {
    return ServerSocketFactory.getDefault();
  }

  /**
   * Signal another thread that we have established a connection This is useful for testing
   * purposes.
   */
  void signalAlmostReadiness() {
    if (latch != null && latch.getCount() != 0) {
      latch.countDown();
    }
  }

  /**
   * Used for testing purposes
   *
   * @param latch
   */
  void setLatch(CountDownLatch latch) {
    this.latch = latch;
  }

  /** Used for testing purposes */
  public CountDownLatch getLatch() {
    return latch;
  }

  public boolean isClosed() {
    return closed;
  }

  public void close() {
    closed = true;
    if (serverSocket != null) {
      try {
        serverSocket.close();
      } catch (IOException e) {
        logger.error("Failed to close serverSocket", e);
      } finally {
        serverSocket = null;
      }
    }

    logger.info("closing this server");
    synchronized (socketNodeList) {
      for (SocketLoggingNode sn : socketNodeList) {
        sn.close();
      }
    }
    if (socketNodeList.size() != 0) {
      logger.warn("Was expecting a 0-sized socketNodeList after server shutdown");
    }
  }

  public void socketNodeClosing(SocketLoggingNode sn) {
    logger.debug("Removing {}", sn);

    // don't allow simultaneous access to the socketNodeList
    // (e.g. removal whole iterating on the list causes
    // java.util.ConcurrentModificationException)
    synchronized (socketNodeList) {
      socketNodeList.remove(sn);
    }
  }

  public static void configureLC(LoggerContext lc, String configFile) throws JoranException {
    JoranConfigurator configurator = new JoranConfigurator();
    lc.reset();
    configurator.setContext(lc);
    configurator.doConfigure(configFile);
  }
}
