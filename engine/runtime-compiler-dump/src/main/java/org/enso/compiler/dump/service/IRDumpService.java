package org.enso.compiler.dump.service;

import java.io.File;
import org.enso.compiler.core.ir.Module;
import org.enso.pkg.Package;

public interface IRDumpService {

  /**
   * @param ir IR of the module to dump
   * @param moduleName Fully-qualified module name
   * @param pkg Package where the module is located. May be null.
   */
  void dump(Module ir, String moduleName, Package<File> pkg);
}
