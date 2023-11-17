package org.enso.profiling.snapshot;

import com.sun.management.HotSpotDiagnosticMXBean;
import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Path;
import javax.management.MBeanServer;

public final class HeapDumpGenerator {

  private static final String HOT_SPOT_BEAN_NAME = "com.sun.management:type=HotSpotDiagnostic";
  private static volatile HotSpotDiagnosticMXBean hotSpotDiagnosticMXBean;

  /**
   * Store the heap dump in the output file in the same format as the hprof heap dump.
   *
   * @param output the output file.
   * @param dumpOnlyLiveObjects if {@code true} dump only <i>live</i> objects i.e. objects that are
   *     reachable from others.
   */
  public static void generateHeapDump(Path output, boolean dumpOnlyLiveObjects) throws IOException {
    if (hotSpotDiagnosticMXBean == null) {
      synchronized (HeapDumpGenerator.class) {
        hotSpotDiagnosticMXBean = getHotSpotDiagnosticMXBean();
      }
    }

    hotSpotDiagnosticMXBean.dumpHeap(output.toString(), dumpOnlyLiveObjects);
  }

  private static HotSpotDiagnosticMXBean getHotSpotDiagnosticMXBean() throws IOException {
    MBeanServer mBeanServer = ManagementFactory.getPlatformMBeanServer();
    return ManagementFactory.newPlatformMXBeanProxy(
        mBeanServer, HOT_SPOT_BEAN_NAME, HotSpotDiagnosticMXBean.class);
  }
}