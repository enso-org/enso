package org.enso.compiler.dump.service;

import java.util.ServiceLoader;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

final class IRDumpSingleton {
  static final IRDumpFactoryService<?> DEFAULT = find();

  private static IRDumpFactoryService<?> find() {
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

  private static final class NoDumping extends IRDumpFactoryService<Void> {
    @Override
    public Void create(String moduleName) {
      return null;
    }

    @Override
    public void shutdown() {}

    @Override
    public void dumpModule(Void group, IRSource<Module> ir) {}

    @Override
    public void dumpExpression(Void group, IRSource<Expression> ir) {}

    @Override
    public void close(Void group) {}
  }
}
