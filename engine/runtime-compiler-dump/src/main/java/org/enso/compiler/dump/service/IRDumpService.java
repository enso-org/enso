package org.enso.compiler.dump.service;

import java.io.File;
import org.enso.compiler.core.ir.Module;

public interface IRDumpService {

  /**
   * @param ir IR of the module to dump
   * @param moduleName Fully-qualified module name
   * @param srcFile Source file of the module. May be null.
   * @param afterPass Name of the pass that this dumper runs after.
   */
  void dump(Module ir, String moduleName, File srcFile, String afterPass);

  /** Close and flush all the graphs. */
  void close();
}
