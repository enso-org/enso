package org.enso.compiler.dump.igv;

import org.graalvm.graphio.GraphStructure;
import org.enso.compiler.core.IR;
import org.enso.compiler.dump.service.IRDumpService;

public final class IGVDumper implements IRDumpService {
  @Override
  public void dump(IR ir) {
    System.out.println("Dumping IR to IGV format");
  }
}
