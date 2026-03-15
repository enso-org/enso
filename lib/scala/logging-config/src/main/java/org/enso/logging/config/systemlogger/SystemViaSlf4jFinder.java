package org.enso.logging.config.systemlogger;

import java.util.Objects;
import java.util.function.Consumer;
import org.slf4j.LoggerFactory;

/** Bridges logs sent to {@link System.Logger} to slf4j logger. */
public final class SystemViaSlf4jFinder extends System.LoggerFinder
    implements Consumer<System.LoggerFinder> {
  private volatile System.LoggerFinder delegate;

  public SystemViaSlf4jFinder() {}

  @Override
  public System.Logger getLogger(String name, Module module) {
    if (delegate != null) {
      var logger = delegate.getLogger(name, module);
      if (logger != null) {
        return logger;
      }
    }
    var logger = LoggerFactory.getLogger(name);
    return new SystemViaSlf4jLogger(logger);
  }

  @Override
  public void accept(System.LoggerFinder t) {
    Objects.requireNonNull(t);
    assert this.delegate == null;
    this.delegate = t;
  }
}
