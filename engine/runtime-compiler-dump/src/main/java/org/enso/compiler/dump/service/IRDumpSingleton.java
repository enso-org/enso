package org.enso.compiler.dump.service;

import java.io.File;
import java.util.ServiceLoader;
import org.enso.compiler.core.ir.Module;

final class IRDumpSingleton {
  static final IRDumpFactoryService DEFAULT = find();

  private static IRDumpFactoryService find() {
    IRDumpFactoryService service = new NoDumping();
    var loader = ServiceLoader.load(IRDumpFactoryService.class);
    var it = loader.iterator();
    while (it.hasNext()) {
      service = it.next();
      assert service != null;
      break;
    }
    return service;
  }

  private static final class NoDumping implements IRDumpFactoryService, IRDumper {
    @Override
    public IRDumper create(String moduleName) {
      return this;
    }

    @Override
    public void shutdown() {}

    @Override
    public void dumpModule(Module ir, String graphName, File srcFile, String afterPass) {}

    @Override
    public void close() {}
  }
}
