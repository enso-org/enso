package org.enso.logging.service.logback;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.net.server.HardenedLoggingEventInputStream;
import ch.qos.logback.classic.spi.ILoggingEvent;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.UUID;

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
  // 0 - not started
  // 1 - running
  // 2 - closing
  // 3 - closed
  volatile int state = 0;
  SocketServer socketServer;
  UUID projectId;

  public SocketLoggingNode(SocketServer socketServer, Socket socket, LoggerContext context) {
    this.socketServer = socketServer;
    this.socket = socket;
    remoteSocketAddress = socket.getRemoteSocketAddress();
    this.context = context;
    logger = context.getLogger(SocketLoggingNode.class);
    projectId = null;
  }

  public void run() {

    try {
      hardenedLoggingEventInputStream =
          new HardenedLoggingEventInputStream(new BufferedInputStream(socket.getInputStream()));
    } catch (Exception e) {
      logger.error("Could not open ObjectInputStream to " + socket, e);
      state = 3;
    }

    ILoggingEvent event;
    Logger remoteLogger;

    try {
      state = 1;
      while (state != 3) {
        // read an event from the wire
        // System.out.println("Reading event?");
        event = (ILoggingEvent) hardenedLoggingEventInputStream.readObject();
        if (projectId == null) {
          try {
            projectId = UUID.fromString(event.getMDCPropertyMap().get("project.id"));
          } catch (IllegalArgumentException e) {
            // ignore
          }
        }
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
      if (state < 2) {
        logger.debug("Caught java.io.EOFException closing connection.", e);
      }
    } catch (java.net.SocketException e) {
      if (state < 2) {
        logger.warn("Caught java.net.SocketException closing connection.");
      }
    } catch (IOException e) {
      if (state < 2) {
        logger.debug("Caught java.io.IOException: " + e);
        logger.debug("Closing connection.");
      }
    } catch (Exception e) {
      if (state < 2) {
        logger.error("Unexpected exception. Closing connection.", e);
      }
    }

    socketServer.socketNodeClosing(this);
    close();
  }

  void closing() {
    if (state < 2) {
      state = 2;
    }
  }

  void close() {
    if (state == 3) {
      return;
    }
    projectId = null;
    state = 3;
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
