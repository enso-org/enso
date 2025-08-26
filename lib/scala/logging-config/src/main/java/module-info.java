import org.enso.logging.config.systemlogger.ContextLoggingViaSlf4j;
import org.enso.logging.config.systemlogger.SystemLoggerViaSlf4j;

module org.enso.logging.config {
  requires org.slf4j;
  requires java.logging;
  requires typesafe.config;
  requires org.enso.logging.utils;
  requires org.enso.engine.common;
  requires static org.graalvm.polyglot;

  exports org.enso.logging.config;

  uses org.enso.logging.config.LoggerSetup;

  provides java.lang.System.LoggerFinder with
      SystemLoggerViaSlf4j;
  provides org.enso.common.ContextLoggingConfigurator with
      ContextLoggingViaSlf4j;
}
