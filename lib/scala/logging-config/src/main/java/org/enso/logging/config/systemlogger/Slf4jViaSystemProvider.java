package org.enso.logging.config.systemlogger;

import org.slf4j.ILoggerFactory;
import org.slf4j.IMarkerFactory;
import org.slf4j.Logger;
import org.slf4j.spi.MDCAdapter;
import org.slf4j.spi.SLF4JServiceProvider;

/**
 * Bridges slf4j logger to {@link System.Logger}. Needs to be enabled by setting JVM properties.
 * Those are set when booting <em>Other JVM</em> in the <em>dual JVM</em> mode.
 */
public final class Slf4jViaSystemProvider implements SLF4JServiceProvider, ILoggerFactory {

  @Override
  public ILoggerFactory getLoggerFactory() {
    return this;
  }

  @Override
  public IMarkerFactory getMarkerFactory() {
    return null;
  }

  @Override
  public MDCAdapter getMDCAdapter() {
    return null;
  }

  @Override
  public String getRequestedApiVersion() {
    return "2.0.16";
  }

  @Override
  public void initialize() {}

  @Override
  public Logger getLogger(String name) {
    var delegate = System.getLogger(name);
    return new Slf4jViaSystemLogger(delegate);
  }
}
