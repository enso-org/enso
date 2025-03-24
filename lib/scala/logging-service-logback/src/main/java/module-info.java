import org.enso.logger.ObservedMessage.Service;
import org.enso.logging.config.LoggerSetup;

module org.enso.logging.service.logback {
  requires java.net.http;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires com.fasterxml.jackson.databind;
  requires sentry;
  requires sentry.logback;
  requires org.enso.logging.service;
  requires org.enso.logging.config;
  requires org.enso.logging.utils;
  requires org.slf4j;
  requires static org.openide.util.lookup.RELEASE180;

  exports org.enso.logging.service.logback;

  provides LoggerSetup with
      org.enso.logging.service.logback.LogbackSetup;
  provides Service with
      org.enso.logging.service.logback.LogbackObservingImpl;
}
