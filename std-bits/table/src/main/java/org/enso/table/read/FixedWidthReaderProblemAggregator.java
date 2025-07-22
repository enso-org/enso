package org.enso.table.read;

import org.enso.table.parsing.problems.AdditionalInvalidRows;
import org.enso.table.parsing.problems.InconsistentFixedWidthLengths;
import org.enso.table.parsing.problems.InvalidFixedWidthRow;
import org.enso.table.problems.Problem;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.read.FixedWidthReader.InvalidFixedWidthRowsBehavior;

public class FixedWidthReaderProblemAggregator extends ProblemAggregator {

  private InvalidFixedWidthRowsBehavior invalidRowsBehavior;
  private final boolean warningsAsErrors;
  private long invalidRowsCount;
  private final long invalidRowsLimit = 10;
  private boolean inconsistentLineLengths = false;

  public FixedWidthReaderProblemAggregator(
      ProblemAggregator parent,
      InvalidFixedWidthRowsBehavior invalidRowsBehavior,
      boolean warningsAsErrors) {
    super(parent);
    this.invalidRowsBehavior = invalidRowsBehavior;
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

  public void reportShortLine(
      long sourceLineNumber, Long tableRowNumber, long lineLength, long minimumLineLength) {
    if (invalidRowsCount < invalidRowsLimit) {
      var tableRowNumberMaybe =
          invalidRowsBehavior == InvalidFixedWidthRowsBehavior.KEEP ? tableRowNumber : null;
      report(
          new InvalidFixedWidthRow(
              sourceLineNumber, tableRowNumberMaybe, lineLength, minimumLineLength));
    }

    invalidRowsCount++;
  }

  public void reportInconsistentLineLengths() {
    inconsistentLineLengths = true;
  }

  @Override
  public ProblemSummary summarize() {
    var summary = super.summarize();
    if (invalidRowsCount > invalidRowsLimit) {
      long additionalInvalidRows = invalidRowsCount - invalidRowsLimit;
      summary.add(new AdditionalInvalidRows(additionalInvalidRows));
    }
    if (inconsistentLineLengths) {
      summary.add(new InconsistentFixedWidthLengths());
    }
    return summary;
  }
}
