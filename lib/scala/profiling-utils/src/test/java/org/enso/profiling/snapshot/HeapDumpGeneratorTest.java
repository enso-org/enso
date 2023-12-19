package org.enso.profiling.snapshot;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.junit.Assert;
import org.junit.Test;

public class HeapDumpGeneratorTest {

  @Test
  public void generateHeapDump() throws IOException {
    String heapDumpFileName =
        getClass().getSimpleName() + "-" + System.currentTimeMillis() + ".hprof";
    Path heapDumpFile = Paths.get("").toRealPath().resolve(heapDumpFileName);

    try {
      HeapDumpGenerator.generateHeapDump(heapDumpFile, true);
      Assert.assertTrue(Files.exists(heapDumpFile));
    } finally {
      Files.deleteIfExists(heapDumpFile);
    }
  }
}
