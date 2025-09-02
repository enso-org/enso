import org.enso.logging.config.LoggerSetup;

module org.enso.launcher {
  requires org.enso.logging.config;
  requires org.enso.logging.service;

  uses LoggerSetup;
}