package org.enso.compiler.dump.service;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;

public interface IRDumpService {

  /**
   *
   * @param ir IR of the module to dump
   * @param moduleName Fully-qualified module name
   */
  void dump(Module ir, String moduleName);
}
