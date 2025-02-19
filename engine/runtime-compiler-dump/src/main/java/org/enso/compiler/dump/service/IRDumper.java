package org.enso.compiler.dump.service;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

public interface IRDumper {

  /**
   * Dumps module IR.
   *
   * @param dump what to dump
   */
  void dumpModule(IRSource<Module> dump);

  /** Dumps a single expression IR. */
  default void dumpExpression(IRSource<Expression> dump) {
    // nop
  }

  /**
   * Close and flush all the underlying resources. There will be no more dumps for the module after
   * this call.
   */
  void close();
}
