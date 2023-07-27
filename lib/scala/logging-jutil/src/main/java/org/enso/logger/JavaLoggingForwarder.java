package org.enso.logger;

import static ch.qos.logback.core.net.SocketConnector.ExceptionHandler;
import static java.util.logging.Level.*;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.net.SocketAppender;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.LoggingEvent;
import ch.qos.logback.classic.spi.ThrowableProxy;
import ch.qos.logback.core.ConsoleAppender;
import ch.qos.logback.core.net.DefaultSocketConnector;
import ch.qos.logback.core.net.SocketConnector;
import java.io.IOException;
import java.net.ConnectException;
import java.net.InetAddress;
import java.net.Socket;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import org.enso.logger.config.LoggingService;

/**
 * JavaLoggingForwarder will forward all java.util.logging.LogRecord to a Logback's appender.
 * Forwarding is dependent upon the presence of the coordinates for the central logging server and
 * wheteher it is up and running.
 */
public class JavaLoggingForwarder extends Handler {
  private SocketConnector connector;

  private ch.qos.logback.core.Appender<ILoggingEvent> appender;
  private boolean started;

  public JavaLoggingForwarder(int port) {
    this("localhost", port);
  }

  public JavaLoggingForwarder(String host, int port) {
    try {
      InetAddress address =
          host.equals("localhost") ? InetAddress.getLocalHost() : InetAddress.getByName(host);
      connector = new DefaultSocketConnector(address, port, 0, 10000);
      connector.setExceptionHandler(new FallbackExceptionHandler(1));
    } catch (Throwable e) {
      connector = null;
    }
    boolean enabled = canEstablishSocketConnection();
    if (enabled) {
      SocketAppender socketAppender = new SocketAppender();
      socketAppender.setPort(port);
      socketAppender.setRemoteHost(host);
      socketAppender.setIncludeCallerData(false);
      LoggerContext ctx = new LoggerContext();
      socketAppender.setContext(new LoggerContext());
      appender = socketAppender;
    } else {
      appender = new ConsoleAppender<ILoggingEvent>();
    }
    appender.addFilter(ApplicationFilter.fromLoggers(LoggingService.parseConfig().getLoggers()));

    started = false;
  }

  @Override
  public void publish(LogRecord record) {
    if (!started) {
      synchronized (this) {
        if (!started) {
          started = true;
          appender.start();
        }
      }
    }
    appender.doAppend(toEvent(record));
  }

  private boolean canEstablishSocketConnection() {
    if (connector == null) return false;
    try {
      Socket socket = connector.call();
      boolean test = socket != null;
      if (test) {
        socket.close();
      }
      return test;
    } catch (InterruptedException | IOException e) {
      System.err.println("Failed to establish connection: " + e.getCause().getMessage());
      return false;
    } catch (RuntimeException e) {
      if (!(e.getCause() instanceof ConnectException)) {
        System.err.println("Failed to establish connection: " + e.getCause().getMessage());
      }
      return false;
    }
  }

  @Override
  public void flush() {}

  @Override
  public void close() throws SecurityException {
    // flush all remaining events
    if (appender != null) appender.stop();
  }

  private ILoggingEvent toEvent(LogRecord record) {
    LoggingEvent event = new LoggingEvent();
    event.setLoggerName(record.getLoggerName());
    event.setLevel(toLogbackLevel(record.getLevel()));
    event.setMessage(record.getMessage());
    event.setArgumentArray(record.getParameters());
    event.setInstant(record.getInstant());
    if (record.getThrown() != null) {
      event.setThrowableProxy(new ThrowableProxy(record.getThrown()));
    }
    event.prepareForDeferredProcessing();
    return event;
  }

  private static ch.qos.logback.classic.Level toLogbackLevel(java.util.logging.Level julLevel) {
    if (julLevel.intValue() == SEVERE.intValue()) return ch.qos.logback.classic.Level.ERROR;
    else if (julLevel.intValue() == INFO.intValue()) return ch.qos.logback.classic.Level.INFO;
    else if (julLevel.intValue() == WARNING.intValue()) return ch.qos.logback.classic.Level.WARN;
    else if (julLevel.intValue() == ALL.intValue()) return ch.qos.logback.classic.Level.ALL;
    else if (julLevel.intValue() == FINE.intValue()) return ch.qos.logback.classic.Level.DEBUG;
    else if (julLevel.intValue() == FINER.intValue()) return ch.qos.logback.classic.Level.TRACE;
    else if (julLevel.intValue() == FINEST.intValue()) return ch.qos.logback.classic.Level.TRACE;
    else return ch.qos.logback.classic.Level.OFF;
  }

  /**
   * Local exception handler that propagates a failure to establish the connection. That way,
   * establishing a socket connection is not attempted infinitely on failure.
   */
  private class FallbackExceptionHandler implements ExceptionHandler {
    int retries;

    public FallbackExceptionHandler(int retries) {
      this.retries = retries;
    }

    public FallbackExceptionHandler() {
      this(2);
    }

    public void connectionFailed(SocketConnector connector, Exception ex) {
      if (retries == 0) {
        throw new RuntimeException(ex);
      } else {
        retries = retries - 1;
      }
    }
  }
}
