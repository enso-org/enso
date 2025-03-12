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
import org.enso.logging.service.logback.telemetry.TelemetryAppender;

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
  private static final String TELEMETRY_LOGGER_NAME = "org.enso.telemetry";
  private static final String TELEMETRY_APPENDER_NAME = "telemetry";

  Logger logger;

  enum State {
    NOT_STARTED,
    RUNNING,
    CLOSING,
    CLOSED;

    boolean isBefore(State s) {
      return this.ordinal() < s.ordinal();
    }
  }

  volatile State state = State.NOT_STARTED;
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
    state = State.RUNNING;
    try {
      hardenedLoggingEventInputStream =
          new HardenedLoggingEventInputStream(new BufferedInputStream(socket.getInputStream()));
    } catch (Exception e) {
      logger.error("Could not open ObjectInputStream to " + socket, e);
      state = State.CLOSED;
    }

    ILoggingEvent event;
    Logger remoteLogger;
    try {
      while (state != State.CLOSED) {
        // read an event from the wire
        // System.out.println("Reading event?");
        event = (ILoggingEvent) hardenedLoggingEventInputStream.readObject();
        if (projectId == null) {
          try {
            var property = event.getMDCPropertyMap().get("project.id");
            if (property != null) {
              projectId = UUID.fromString(property);
            }
          } catch (IllegalArgumentException e) {
            // ignore
          }
        }
        if (event.getLoggerName().startsWith(TELEMETRY_LOGGER_NAME)) {
          logTelemetryEvent(event);
        } else {
          // get a logger from the hierarchy. The name of the logger is taken to
          // be the name contained in the event.
          remoteLogger = context.getLogger(event.getLoggerName());
          // apply the logger-level filter
          if (remoteLogger.isEnabledFor(event.getLevel())) {
            // finally log the event as if was generated locally
            remoteLogger.callAppenders(event);
          }
        }
      }
    } catch (java.io.EOFException e) {
      if (state.isBefore(State.CLOSING)) {
        logger.debug("Caught java.io.EOFException closing connection.", e);
      }
    } catch (java.net.SocketException e) {
      if (state.isBefore(State.CLOSING)) {
        logger.warn("Caught java.net.SocketException closing connection.");
      }
    } catch (IOException e) {
      if (state.isBefore(State.CLOSING)) {
        logger.debug("Caught java.io.IOException: " + e);
        logger.debug("Closing connection.");
      }
    } catch (Exception e) {
      if (state.isBefore(State.CLOSING)) {
        logger.error("Unexpected exception. Closing connection.", e);
      }
    }

    socketServer.socketNodeClosing(this);
    close();
  }

  void closing() {
    if (state.isBefore(State.CLOSING)) {
      state = State.CLOSING;
    }
  }

  void close() {
    if (state == State.CLOSED) {
      return;
    }
    projectId = null;
    state = State.CLOSED;
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

  private void logTelemetryEvent(ILoggingEvent event) {
    var telemetryLogger = context.getLogger(TELEMETRY_LOGGER_NAME);
    var telemetryAppender = telemetryLogger.getAppender(TELEMETRY_APPENDER_NAME);
    if (telemetryAppender == null) {
      telemetryAppender = TelemetryAppender.create();
      telemetryAppender.setContext(context);
      telemetryAppender.setName(TELEMETRY_APPENDER_NAME);
      telemetryAppender.start();
      telemetryLogger.addAppender(telemetryAppender);
    }
    telemetryLogger.callAppenders(event);
  }

  @Override
  public String toString() {
    return this.getClass().getName() + remoteSocketAddress.toString();
  }
}
