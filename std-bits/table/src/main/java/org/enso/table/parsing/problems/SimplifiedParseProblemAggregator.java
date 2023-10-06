package org.enso.table.parsing.problems;

public final class SimplifiedParseProblemAggregator implements ParseProblemAggregator {

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
