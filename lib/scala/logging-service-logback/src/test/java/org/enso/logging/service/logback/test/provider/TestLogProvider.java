package org.enso.logging.service.logback.test.provider;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.LogbackServiceProvider;
import org.enso.logging.config.MissingConfigurationField;
import org.enso.logging.service.logback.LogbackSetup;
import org.enso.logging.service.logback.MemoryAppender;
import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.Logger;
import org.slf4j.spi.MDCAdapter;
import org.slf4j.spi.SLF4JServiceProvider;

@org.openide.util.lookup.ServiceProvider(service = SLF4JServiceProvider.class)
public class TestLogProvider implements SLF4JServiceProvider {

  private static final SLF4JServiceProvider underlying = new LogbackServiceProvider();

  private boolean initialized = false;

  private static MemoryAppender memoryAppender = null;

  public static MemoryAppender getMemoryAppender(ILoggerFactory ctx) {
    if (memoryAppender == null) {
      var rootLogger = (ch.qos.logback.classic.Logger) (ctx.getLogger(Logger.ROOT_LOGGER_NAME));
      var appender = rootLogger.getAppender(MemoryAppender.NAME);
      if (appender instanceof MemoryAppender a) {
        memoryAppender = a;
      }
    }
    return memoryAppender;
  }

  @Override
  public ILoggerFactory getLoggerFactory() {
    ILoggerFactory factory = underlying.getLoggerFactory();
    assert factory instanceof LoggerContext;
    if (!initialized) {
      try {
        var setup = new LogbackSetup((LoggerContext) factory);
        setup.setup();
        initialized = true;
      } catch (MissingConfigurationField e) {
        throw new RuntimeException(e);
      }
    }
    return factory;
  }

  @Override
  public IMarkerFactory getMarkerFactory() {
    return underlying.getMarkerFactory();
  }

  @Override
  public MDCAdapter getMDCAdapter() {
    return underlying.getMDCAdapter();
  }

  @Override
  public String getRequestedApiVersion() {
    return underlying.getRequestedApiVersion();
  }

  @Override
  public void initialize() {
    underlying.initialize();
  }
}
