package org.enso.compiler.dump.igv;

import org.enso.compiler.dump.service.IRDumpFactoryService;
import org.enso.compiler.dump.service.IRDumper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IGVDumperFactory implements IRDumpFactoryService {

  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumperFactory.class);

  @Override
  public IRDumper create(String moduleName) {
    LOGGER.trace("Creating IGV dumper for module {}", moduleName);
    return new IGVDumper(moduleName);
  }
}
