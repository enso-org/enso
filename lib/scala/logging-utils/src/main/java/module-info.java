module org.enso.logging.utils {
  requires scala.library;
  requires java.logging;
  requires org.slf4j;

  exports org.enso.logger;
  exports org.enso.logger.masking;
}
