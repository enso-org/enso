package org.enso.interpreter.runtime.control;

/** Thrown when guest code discovers a thread interrupt. */
public class ThreadInterruptedException extends RuntimeException {
  public ThreadInterruptedException() {}

  public ThreadInterruptedException(Throwable e) {
    super(e);
  }
}
