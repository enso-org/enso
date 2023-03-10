package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

import java.util.List;

/** A problem aggregator which ignores problems. */
public class NoOpProblemAggregator implements ProblemAggregator {

  @Override
  public void reportInvalidFormat(String cell) {}

  @Override
  public void reportLeadingZeroes(String cell) {}

  @Override
  public void reportMismatchedQuote(String cellText) {}

  @Override
  public boolean hasProblems() {
    throw new IllegalStateException("This implementation does not provide problem information.");
  }

  @Override
  public List<Problem> getAggregatedProblems() {
    throw new IllegalStateException("This implementation does not provide problem information.");
  }
}
