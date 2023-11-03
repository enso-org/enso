package org.enso.polyglot.debugger;

import java.util.UUID;

public final class DefaultExecutionSupport implements IdExecutionService.ExecutionSupport {

  private static final DefaultExecutionSupport INSTANCE = new DefaultExecutionSupport();

  private DefaultExecutionSupport() {}

  public static IdExecutionService.ExecutionSupport getInstance() {
    return INSTANCE;
  }

  @Override
  public Object findCachedResult(Object materializedFrame, Object node, UUID nodeId) {
    return null;
  }

  @Override
  public void updateCachedResult(UUID nodeId, Object result, boolean isPanic, long nanoElapsedTime) {}

}
