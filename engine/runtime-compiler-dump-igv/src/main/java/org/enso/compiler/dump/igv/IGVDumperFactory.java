package org.enso.compiler.dump.igv;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import org.enso.compiler.dump.service.IRDumpFactoryService;
import org.enso.compiler.dump.service.IRDumper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IGVDumperFactory implements IRDumpFactoryService {

  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumperFactory.class);
  private final ExecutorService executor;

  public IGVDumperFactory() {
    this.executor = Executors.newVirtualThreadPerTaskExecutor();
  }

  @Override
  public IRDumper create(String moduleName) {
    LOGGER.trace("Creating IGV dumper for module {}", moduleName);
    return IGVDumper.createForModule(moduleName, executor);
  }

  @Override
  public void shutdown() {
    executor.shutdown();
  }
}
