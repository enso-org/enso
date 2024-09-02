package org.enso.interpreter.instrument.execution;

public interface ExecutionHooks {

  /**
   * Add a hook to run before the execution.
   *
   * @param hook the runnable hook
   */
  void add(Runnable hook);

  /**
   * Consume and run all the stored execution hooks.
   */
  void run();
}
