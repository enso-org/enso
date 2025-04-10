package org.enso.table.read;

import org.enso.table.problems.Problem;
import org.enso.table.problems.ProblemAggregator;

public class FixedWidthReaderProblemAggregator extends ProblemAggregator {
  private final boolean warningsAsErrors;
  private final char quoteCharacter;

  public FixedWidthReaderProblemAggregator(
      ProblemAggregator parent,
      boolean warningsAsErrors) {
    super(parent);
    this.warningsAsErrors = warningsAsErrors;
  }

  @Override
  public void report(Problem problem) {
    if (warningsAsErrors) {
      throw new FixedWidthReadFailedException(problem);
    } else {
      super.report(problem);
    }
  }
}
