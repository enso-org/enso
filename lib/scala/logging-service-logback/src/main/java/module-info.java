import org.enso.logger.ObservedMessage.Service;
import org.enso.logging.config.LoggerSetup;
import org.enso.logging.service.LoggingServiceFactory;
import org.enso.logging.service.logback.LogbackLoggingServiceFactory;

module org.enso.logging.service.logback {
  requires java.net.http;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires org.enso.logging.service;
  requires org.enso.logging.config;
  requires org.enso.logging.utils;
  requires org.slf4j;

  exports org.enso.logging.service.logback;

  uses org.enso.logging.service.logback.AbstractRemoteAppender;

  provides LoggerSetup with
      org.enso.logging.service.logback.LogbackSetup;
  provides Service with
      org.enso.logging.service.logback.LogbackObservingImpl;
  provides LoggingServiceFactory with
      LogbackLoggingServiceFactory;
}
