import org.slf4j.spi.SLF4JServiceProvider;

module org.enso.logging.service.logback.test.provider {
  requires scala.library;
  requires ch.qos.logback.classic;
  requires ch.qos.logback.core;
  requires org.enso.logging.service;
  requires org.enso.logging.service.logback;
  requires org.enso.logging.config;
  requires org.slf4j;
  requires org.openide.util.lookup.RELEASE180;

  provides SLF4JServiceProvider with
      org.enso.logging.service.logback.test.provider.TestLogProvider;
  
  exports org.enso.logging.service.logback.test.provider;
}
