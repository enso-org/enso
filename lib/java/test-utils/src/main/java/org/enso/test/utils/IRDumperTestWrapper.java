package org.enso.test.utils;

import java.util.HashMap;
import java.util.Map;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumpFactoryService;
import org.enso.compiler.dump.service.IRSource;

/** Utility class for {@link org.enso.compiler.dump.service.IRDumper}. */
public final class IRDumperTestWrapper implements AutoCloseable {
  private final Map<String, org.enso.compiler.dump.service.IRDumper> dumpers = new HashMap<>();

  /**
   * @param ir Either {@link Module} or {@link Expression}. If it is Expression, a synthetic Module
   *     IR is created and dumped.
   * @param moduleName
   * @param passName
   */
  public void dump(IR ir, String moduleName, String passName) {
    var dumper = dumpers.get(moduleName);
    if (dumper == null) {
      dumper = IRDumpFactoryService.DEFAULT.create(moduleName);
      dumpers.put(moduleName, dumper);
    }
    if (ir instanceof Module modIr) {
      var src = new IRSource<Module>(modIr, moduleName, passName, null, null);
      dumper.dumpModule(src);
    } else if (ir instanceof Expression expr) {
      var src = new IRSource<Expression>(expr, moduleName, passName, null, null);
      dumper.dumpExpression(src);
    } else {
      throw new IllegalArgumentException("Unsupported IR type: " + ir.getClass());
    }
  }

  @Override
  public void close() throws Exception {
    dumpers.values().forEach(org.enso.compiler.dump.service.IRDumper::close);
    dumpers.clear();
  }
}
