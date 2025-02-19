package org.enso.compiler.dump.igv;

import org.enso.compiler.dump.service.IRDumpFactoryService;
import org.enso.compiler.dump.service.IRDumper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IGVDumperFactory implements IRDumpFactoryService {
  private static final Logger LOGGER = LoggerFactory.getLogger(IGVDumperFactory.class);
  private static final String GRAPHIO_PKG = "org.graalvm.graphio";
  private static final String COMPILER_MOD = "jdk.internal.vm.compiler";

  public IGVDumperFactory() {
    ensureInternalModuleIsExported();
  }

  private static void ensureInternalModuleIsExported() {
    var internalCompilerMod = ModuleLayer.boot().findModule(COMPILER_MOD);
    if (internalCompilerMod.isEmpty()) {
      throw new IllegalStateException(
          "Module " + COMPILER_MOD + " is not present. Is this a GraalVM JDK?");
    }
    var thisModule = IGVDumperFactory.class.getModule();
    if (!internalCompilerMod.get().isExported(GRAPHIO_PKG, thisModule)) {
      throw new IllegalStateException(
          "Package "
              + GRAPHIO_PKG
              + " is not exported to this module. You must explicitly provide "
              + "--add-exports %s/%s=%s"
                  .formatted(COMPILER_MOD, GRAPHIO_PKG, thisModule.getName()));
    }
  }

  @Override
  public IRDumper create(String moduleName) {
    LOGGER.trace("Creating IGV dumper for module {}", moduleName);
    return IGVDumper.createForModule(moduleName);
  }

  @Override
  public void shutdown() {}
}
