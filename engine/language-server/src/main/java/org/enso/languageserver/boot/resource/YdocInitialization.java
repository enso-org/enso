package org.enso.languageserver.boot.resource;

import java.util.concurrent.Executor;
import org.enso.ydoc.Ydoc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class YdocInitialization extends LockedInitialization {

  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private final Ydoc ydoc;

  public YdocInitialization(Executor executor, Ydoc ydoc) {
    super(executor);
    this.ydoc = ydoc;
  }

  @Override
  public void initComponent() {
    logger.info("Starting Ydoc server...");
    try {
      ydoc.start();
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    logger.info("Started Ydoc server.");
  }
}
