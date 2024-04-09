package org.enso.profiling.snapshot;

import java.io.IOException;
import java.nio.file.Path;

public interface ProfilingSnapshot {

  /**
   * Generate the profiling snapshot in the provided location.
   *
   * @param output the output file.
   */
  void generateSnapshot(Path output) throws IOException;
}
