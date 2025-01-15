package org.enso.compiler.dump.igv;

import org.enso.compiler.core.ir.Module;
import org.enso.compiler.dump.service.IRDumpService;

public final class IGVDumper implements IRDumpService {

  @Override
  public void dump(Module ir, String moduleName) {
    System.out.println("Dumping IR for module " + moduleName + " in IGV");
  }
}
