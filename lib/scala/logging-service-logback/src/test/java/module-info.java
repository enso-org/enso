module org.enso.logging.test {
  requires org.slf4j;
  requires ch.qos.logback.classic;
  provides org.slf4j.spi.SLF4JServiceProvider with org.enso.logger.TestLogProvider;
}
