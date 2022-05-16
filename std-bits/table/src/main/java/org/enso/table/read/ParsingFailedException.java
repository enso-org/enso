package org.enso.table.read;

/**
 * An exception thrown when a problem occured during parsing and the parser is running in a mode
 * that does not try recovering, so the parsing is stopped.
 */
public class ParsingFailedException extends RuntimeException {
  public final ParsingProblem problem;

  public ParsingFailedException(ParsingProblem problem) {
    this.problem = problem;
  }
}
