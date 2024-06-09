package org.enso.table.parsing.problems;

/**
 * A simple implementation of {@link ParseProblemAggregator} that only tracks if a problem has
 * occurred.
 *
 * <p>It allows to track various branches of parsing attempts, without storing additional
 * information about the problems - useful when we only care if there are any problems at all, but
 * not about their contents.
 */
public final class ShortCircuitParseProblemAggregator implements ParseProblemAggregator {

  private boolean hasProblems = false;

  @Override
  public void reportInvalidFormat(String cell) {
    hasProblems = true;
  }

  @Override
  public void reportMismatchedQuote(String cellText) {
    hasProblems = true;
  }

  @Override
  public boolean hasProblems() {
    return hasProblems;
  }
}
