package org.enso.logging.service.logback;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.net.server.HardenedLoggingEventInputStream;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.LoggingEvent;
import ch.qos.logback.classic.spi.ThrowableProxy;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;
import java.time.Instant;
import java.util.Map;
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
  Map<String, String> localMdc;

  public SocketLoggingNode(SocketServer socketServer, Socket socket, LoggerContext context) {
    this.socketServer = socketServer;
    this.socket = socket;
    remoteSocketAddress = socket.getRemoteSocketAddress();
    this.context = context;
    logger = context.getLogger(SocketLoggingNode.class);
    projectId = null;
    localMdc = null;
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
        try {
          event = (ILoggingEvent) hardenedLoggingEventInputStream.readObject();
          if (projectId == null) {
            try {
              var property = event.getMDCPropertyMap().get("projectLocalId");
              if (property != null) {
                projectId = UUID.fromString(property);
                localMdc = event.getMDCPropertyMap();
              }
            } catch (IllegalArgumentException e) {
              // ignore
            }
          }
          // get a logger from the hierarchy. The name of the logger is taken to
          // be the name contained in the event.
          remoteLogger = context.getLogger(event.getLoggerName());
          // apply the logger-level filter
          if (remoteLogger.isEnabledFor(event.getLevel())) {
            // Ensure MDC properties are set
            // event.getMDCPropertyMap() returns an immutable map that can't be updated
            var event1 = localMdc != null ? new ProxyLoggingEvent(event, localMdc) : event;
            // finally log the event as if was generated locally
            remoteLogger.callAppenders(event1);
          }
        } catch (IOException e) {
          throw e;
        } catch (Throwable e) {
          var loggingEvent = new LoggingEvent();
          loggingEvent.setLevel(Level.ERROR);
          if (e.getStackTrace().length > 0) {
            var name = e.getStackTrace()[0].getClassName();
            var nestedClassIdx = name.indexOf("$");
            if (nestedClassIdx > 0) name = name.substring(0, nestedClassIdx);
            loggingEvent.setLoggerName(name);
          } else loggingEvent.setLoggerName(DeferredProcessingSocketAppender.class.getName());
          loggingEvent.setInstant(Instant.now());
          loggingEvent.setThrowableProxy(new ThrowableProxy(e));
          loggingEvent.setMessage("Internal error during deserialization: " + e.getMessage());
          remoteLogger = context.getLogger(loggingEvent.getLoggerName());
          remoteLogger.callAppenders(loggingEvent);
        }
      }
    } catch (java.io.EOFException e) {
      if (state.isBefore(State.CLOSING) && projectId != null) {
        logger.debug("Caught java.io.EOFException closing connection.");
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

  @Override
  public String toString() {
    return this.getClass().getName() + remoteSocketAddress.toString();
  }
}
