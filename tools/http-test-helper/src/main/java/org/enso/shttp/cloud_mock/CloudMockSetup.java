package org.enso.shttp.cloud_mock;

import java.util.List;

public record CloudMockSetup(boolean logBatchingTestModeEnabled) {
  public static CloudMockSetup fromArgs(String[] remainingArgs) {
    List<String> args = List.of(remainingArgs);
    boolean logBatchingTestModeEnabled = args.contains("--enable-manual-log-batching-test");
    return new CloudMockSetup(logBatchingTestModeEnabled);
  }
}
