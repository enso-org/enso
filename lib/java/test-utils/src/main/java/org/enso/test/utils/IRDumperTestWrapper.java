package org.enso.test.utils;

import static org.enso.scala.wrapper.ScalaConversions.cons;
import static org.enso.scala.wrapper.ScalaConversions.nil;

import java.util.HashMap;
import java.util.Map;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.MethodReference;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.dump.service.IRDumpFactoryService;
import org.enso.persist.Persistance.Reference;
import scala.Option;

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
    Module moduleIr;
    if (ir instanceof Module modIr) {
      moduleIr = modIr;
    } else if (ir instanceof Expression expr) {
      moduleIr = createSyntheticModuleIR(expr);
    } else {
      throw new IllegalArgumentException("Unsupported IR type: " + ir.getClass());
    }
    dumper.dump(moduleIr, moduleName, null, passName);
  }

  private static Module createSyntheticModuleIR(Expression expr) {
    var methodName = new Name.Literal("method", true, null, Option.empty(), new MetadataStorage());
    var methodRef = new MethodReference(Option.empty(), methodName, null, new MetadataStorage());
    var method =
        new Method.Explicit(
            methodRef, Reference.of(expr, false), true, false, false, null, new MetadataStorage());
    var mod = new Module(nil(), nil(), cons(method, nil()), false, null, new MetadataStorage());
    return mod;
  }

  @Override
  public void close() throws Exception {
    dumpers.values().forEach(org.enso.compiler.dump.service.IRDumper::close);
    dumpers.clear();
  }
}
