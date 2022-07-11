package org.enso.table.read;

import org.enso.table.problems.Problem;

/**
 * An exception thrown when a problem occured during parsing and the parser is running in a mode
 * that does not try recovering, so the parsing is stopped.
 */
public class ParsingFailedException extends RuntimeException {
  public final Problem problem;

  public ParsingFailedException(Problem problem) {
    this.problem = problem;
  }
}
