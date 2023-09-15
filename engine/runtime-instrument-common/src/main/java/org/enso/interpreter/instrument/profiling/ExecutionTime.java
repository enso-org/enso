package org.enso.interpreter.instrument.profiling;

/** Information on the execution time of an introspected expression. */
public class ExecutionTime implements ProfilingInfo {
  private final long nanoTimeElapsed;

  public ExecutionTime(long nanoTimeElapsed) {
    this.nanoTimeElapsed = nanoTimeElapsed;
  }

  /** @return the time elapsed while executing the expression */
  public long getNanoTimeElapsed() {
    return nanoTimeElapsed;
  }

  @Override
  public String toString() {
    return "ExecutionTime{nanoTimeElapsed=" + nanoTimeElapsed + "}";
  }

  /** @return an execution time representing that no time has passed */
  public static ExecutionTime empty() {
    return new ExecutionTime(0);
  }
}
