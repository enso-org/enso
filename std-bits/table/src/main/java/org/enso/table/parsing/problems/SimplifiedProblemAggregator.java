package org.enso.table.parsing.problems;

import java.util.List;

public class SimplifiedProblemAggregator implements ProblemAggregator {

  private boolean hasProblems = false;

  @Override
  public void reportInvalidFormat(String cell) {
    hasProblems = true;
  }

  @Override
  public void reportLeadingZeroes(String cell) {
    hasProblems = true;
  }

  @Override
  public void reportMismatchedQuote() {
    hasProblems = true;
  }

  @Override
  public boolean hasProblems() {
    return hasProblems;
  }

  @Override
  public List<ParsingProblem> getAggregatedProblems() {
    throw new IllegalStateException("Problem aggregation is not available in this implementation.");
  }
}
