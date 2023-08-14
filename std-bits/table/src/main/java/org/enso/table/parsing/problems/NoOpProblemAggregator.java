package org.enso.table.parsing.problems;

import org.enso.table.problems.AggregatedProblems;

/** A problem aggregator which ignores problems. */
public class NoOpProblemAggregator implements ProblemAggregator {

  @Override
  public void reportInvalidFormat(String cell) {}

  @Override
  public void reportMismatchedQuote(String cellText) {}

  @Override
  public boolean hasProblems() {
    throw new IllegalStateException("This implementation does not provide problem information.");
  }

  @Override
  public AggregatedProblems getAggregatedProblems() {
    throw new IllegalStateException("This implementation does not provide problem information.");
  }
}
