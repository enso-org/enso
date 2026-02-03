package org.enso.table.read;

import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/**
 * An exception thrown when a problem occurred during parsing and the parser is running in a mode
 * that does not try recovering, so the parsing is stopped.
 */
public class ParsingFailedException extends RuntimeException {
  public final Problem problem;

  public ParsingFailedException(Problem problem) {
    this.problem = problem;
  }

  /** Retrieves the problem that caused the exception. */
  public Value asEnsoValue() {
    return problem.asEnsoValue();
  }
}
