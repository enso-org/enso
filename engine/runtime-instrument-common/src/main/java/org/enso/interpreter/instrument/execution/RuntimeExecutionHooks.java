package org.enso.interpreter.instrument.execution;

import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class RuntimeExecutionHooks implements ExecutionHooks {

  private final Logger logger = LoggerFactory.getLogger(getClass());
  private final List<Runnable> hooks = new ArrayList<>();

  public RuntimeExecutionHooks() {}

  @Override
  public void add(Runnable hook) {
    hooks.add(hook);
  }

  @Override
  public void run() {
    while (!hooks.isEmpty()) {
      var hook = hooks.remove(0);
      try {
        hook.run();
      } catch (Exception e) {
        logger.error("Failed to run execution hook.", e);
      }
    }
  }
}
