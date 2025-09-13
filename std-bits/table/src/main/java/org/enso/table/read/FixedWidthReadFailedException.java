package org.enso.table.read;

import org.enso.table.problems.Problem;

/** An exception thrown when a problem occured when reading a fixed-width file. */
public class FixedWidthReadFailedException extends RuntimeException {
  public final Problem problem;

  public FixedWidthReadFailedException(Problem problem) {
    this.problem = problem;
  }
}
