package org.enso.compiler.dump;

import java.io.File;
import java.util.ServiceLoader;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumpService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class IRDumper {
  public static final String SYSTEM_PROP = "enso.compiler.dumpIr";
  public static final String DEFAULT_DUMP_DIR = "ir-dumps";
  private final Logger logger = LoggerFactory.getLogger(IRDumper.class);
  private final IRDumpService dumpService;

  /**
   * @param dumperImpl Class name for the {@link IRDumpService} to use.
   */
  public IRDumper(String dumperImpl) {
    this.dumpService = loadService(dumperImpl);
    if (this.dumpService != null) {
      logger.info("Found IRDumpService: {}", dumperImpl);
    } else {
      logger.error("No IRDumpService found for {}", dumperImpl);
    }
  }

  /**
   * @param moduleIr
   * @param moduleName FQN of the module
   * @param afterPass Name of the pass that this dumping is run after.
   * @param srcFile Source file for the module. May be null.
   */
  public void dumpModule(Module moduleIr, String moduleName, String afterPass, File srcFile) {
    dumpService.dump(moduleIr, moduleName, srcFile, afterPass);
  }

  private static IRDumpService loadService(String implName) {
    var loader = ServiceLoader.load(IRDumpService.class);
    for (IRDumpService service : loader) {
      if (service.getClass().getName().equals(implName)) {
        return service;
      }
    }
    return null;
  }
}
