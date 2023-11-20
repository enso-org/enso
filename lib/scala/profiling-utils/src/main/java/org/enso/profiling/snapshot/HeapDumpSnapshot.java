package org.enso.profiling.snapshot;

import java.io.IOException;
import java.nio.file.Path;

/** Generate the heap dump of the current Java process. */
public final class HeapDumpSnapshot implements ProfilingSnapshot {

  @Override
  public void generateSnapshot(Path output) throws IOException {
    HeapDumpGenerator.generateHeapDump(output, false);
  }
}
