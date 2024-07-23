package org.enso.languageserver.boot.resource;

import java.util.concurrent.Executor;
import org.enso.languageserver.boot.ComponentSupervisor;
import org.enso.languageserver.boot.config.ApplicationConfig;
import org.enso.ydoc.Ydoc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class YdocInitialization extends LockedInitialization {

  private final Logger logger = LoggerFactory.getLogger(this.getClass());
  private final ComponentSupervisor supervisor;

  public YdocInitialization(Executor executor, ComponentSupervisor componentSupervisor) {
    super(executor);
    this.supervisor = componentSupervisor;
  }

  @Override
  public void initComponent() {
    logger.info("Starting Ydoc server...");
    var applicationConfig = ApplicationConfig.load();
    var ydoc =
        Ydoc.builder()
            .hostname(applicationConfig.ydoc().hostname())
            .port(applicationConfig.ydoc().port())
            .build();
    try {
      //ydoc.start();
      this.supervisor.registerService(ydoc);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
    logger.info("Started Ydoc server.");
  }
}
