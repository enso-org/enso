package org.enso.logger;

import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.LogbackServiceProvider;
import org.enso.logger.config.MissingConfigurationField;
import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.spi.MDCAdapter;
import org.slf4j.spi.SLF4JServiceProvider;

@org.openide.util.lookup.ServiceProvider(service = SLF4JServiceProvider.class)
public class TestLogProvider implements SLF4JServiceProvider {

  private static final SLF4JServiceProvider underlying = new LogbackServiceProvider();

  private boolean initialized = false;

  @Override
  public ILoggerFactory getLoggerFactory() {
    ILoggerFactory factory = underlying.getLoggerFactory();
    assert factory instanceof LoggerContext;
    if (!initialized) {
      try {
        new LogbackSetup((LoggerContext) factory).setup();
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
