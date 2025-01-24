package org.enso.compiler.test;

import java.util.function.Function;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.test.utils.IRDumperTestWrapper;

public interface WithIRDumper {
  IRDumperTestWrapper dumper = new IRDumperTestWrapper();

  default Expression processExprWithDump(
      Expression expr, String graphName, Function<Expression, Expression> transition) {
    dumper.dump(expr, graphName, "before");
    var newIr = transition.apply(expr);
    dumper.dump(newIr, graphName, "after");
    return newIr;
  }

  default Module processModuleWithDump(
      Module expr, String graphName, Function<Module, Module> transition) {
    dumper.dump(expr, graphName, "before");
    var newIr = transition.apply(expr);
    dumper.dump(newIr, graphName, "after");
    return newIr;
  }
}
