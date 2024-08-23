import org.enso.logging.config.LoggerSetup;
import org.enso.logging.service.logback.LogbackSetup;

module org.enso.logging.service.logback {
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires sentry;
  requires sentry.logback;
  requires org.enso.logging.service;
  requires org.enso.logging.config;
  requires org.slf4j;
  requires org.openide.util.lookup.RELEASE180;

  provides LoggerSetup with LogbackSetup;
}
