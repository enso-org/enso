package org.enso.table.parsing.problems;

import java.util.List;
import org.enso.table.problems.Problem;

public class SimplifiedProblemAggregator implements ProblemAggregator {

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

  @Override
  public List<Problem> getAggregatedProblems() {
    throw new IllegalStateException("Problem aggregation is not available in this implementation.");
  }
}
