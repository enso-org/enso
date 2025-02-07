package org.enso.compiler.dump.service;

import java.io.File;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

public interface IRDumper {

  /**
   * Dumps module IR.
   *
   * @param ir IR of the module to dump
   * @param graphName Name of the graph to be dumped. Usually a fully-qualified module name.
   * @param srcFile Source file of the module. May be null.
   * @param afterPass Name of the pass that this dumper runs after. Corresponds to name of the
   *     subgraph.
   */
  void dumpModule(Module ir, String graphName, File srcFile, String afterPass);

  /**
   * Dumps a single expression IR.
   *
   * @param expr the IR.
   * @param graphName Name of the graph to be dumped. May contain spaces.
   * @param afterPass Name of the subgraph.
   */
  default void dumpExpression(Expression expr, String graphName, String afterPass) {
    // nop
  }

  /**
   * Close and flush all the underlying resources. There will be no more dumps for the module after
   * this call.
   */
  void close();
}
