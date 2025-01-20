package org.enso.logging.service.logback;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.net.server.HardenedLoggingEventInputStream;
import ch.qos.logback.classic.spi.ILoggingEvent;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;

// Contributors: Moses Hohman <mmhohman@rainbow.uchicago.edu>

/**
 * A direct copy of ch.qos.logback.classic.net.SocketNode. Read {@link ILoggingEvent} objects sent
 * from a remote client using Sockets (TCP). These logging events are logged according to local
 * policy, as if they were generated locally.
 *
 * <p>For example, the socket node might decide to log events to a local file and also resent them
 * to a second socket node.
 *
 * @author Ceki G&uuml;lc&uuml;
 * @author S&eacute;bastien Pennec
 * @since 0.8.4
 */
public class SocketLoggingNode implements Runnable {

  Socket socket;
  LoggerContext context;
  HardenedLoggingEventInputStream hardenedLoggingEventInputStream;
  SocketAddress remoteSocketAddress;

  Logger logger;
  boolean closed = false;
  SocketServer socketServer;

  public SocketLoggingNode(SocketServer socketServer, Socket socket, LoggerContext context) {
    this.socketServer = socketServer;
    this.socket = socket;
    remoteSocketAddress = socket.getRemoteSocketAddress();
    this.context = context;
    logger = context.getLogger(SocketLoggingNode.class);
  }

  public void run() {

    try {
      hardenedLoggingEventInputStream =
          new HardenedLoggingEventInputStream(new BufferedInputStream(socket.getInputStream()));
    } catch (Exception e) {
      logger.error("Could not open ObjectInputStream to " + socket, e);
      closed = true;
    }

    ILoggingEvent event;
    Logger remoteLogger;

    try {
      while (!closed) {
        // read an event from the wire
        // System.out.println("Reading event?");
        event = (ILoggingEvent) hardenedLoggingEventInputStream.readObject();
        // System.out.println("WHAT EVENT? " + event.getMessage());
        // get a logger from the hierarchy. The name of the logger is taken to
        // be the name contained in the event.
        remoteLogger = context.getLogger(event.getLoggerName());
        // apply the logger-level filter
        if (remoteLogger.isEnabledFor(event.getLevel())) {
          // finally log the event as if was generated locally
          remoteLogger.callAppenders(event);
        }
      }
    } catch (java.io.EOFException e) {
      e.printStackTrace();
      logger.debug("Caught java.io.EOFException closing connection.", e);
    } catch (java.net.SocketException e) {
      logger.warn("Caught java.net.SocketException closing connection.");
    } catch (IOException e) {
      logger.debug("Caught java.io.IOException: " + e);
      logger.debug("Closing connection.");
    } catch (Exception e) {
      logger.error("Unexpected exception. Closing connection.", e);
    }

    socketServer.socketNodeClosing(this);
    close();
  }

  void close() {
    if (closed) {
      return;
    }
    closed = true;
    if (hardenedLoggingEventInputStream != null) {
      try {
        hardenedLoggingEventInputStream.close();
      } catch (IOException e) {
        logger.warn("Could not close connection.", e);
      } finally {
        hardenedLoggingEventInputStream = null;
      }
    }
  }

  @Override
  public String toString() {
    return this.getClass().getName() + remoteSocketAddress.toString();
  }
}
