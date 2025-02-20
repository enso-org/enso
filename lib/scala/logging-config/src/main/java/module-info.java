module org.enso.logging.config {
  requires org.slf4j;
  requires typesafe.config;

  exports org.enso.logging.config;

  uses org.enso.logging.config.LoggerSetup;
}
