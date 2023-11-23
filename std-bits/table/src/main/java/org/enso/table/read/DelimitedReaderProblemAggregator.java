package org.enso.table.read;

import org.enso.table.parsing.problems.AdditionalInvalidRows;
import org.enso.table.parsing.problems.InvalidRow;
import org.enso.table.parsing.problems.MismatchedQuote;
import org.enso.table.problems.Problem;
import org.enso.table.problems.ProblemAggregator;

public class DelimitedReaderProblemAggregator extends ProblemAggregator {

  private final boolean warningsAsErrors;
  private final char quoteCharacter;
  private long invalidRowsCount;
  private final long invalidRowsLimit;

  public DelimitedReaderProblemAggregator(
      ProblemAggregator parent,
      boolean warningsAsErrors,
      char quoteCharacter,
      long invalidRowsLimit) {
    super(parent);
    this.warningsAsErrors = warningsAsErrors;
    this.quoteCharacter = quoteCharacter;
    this.invalidRowsCount = 0;
    this.invalidRowsLimit = invalidRowsLimit;
  }

  @Override
  public void report(Problem problem) {
    if (warningsAsErrors) {
      throw new ParsingFailedException(problem);
    } else {
      super.report(problem);
    }
  }

  void reportMismatchedQuote(String cellText) {
    throw new MismatchedQuote(cellText);
  }

  void reportInvalidRow(long source_row, Long table_index, String[] row, long expected_length) {
    // Mismatched quote error takes precedence over invalid row.
    for (int i = 0; i < row.length; i++) {
      String cell = row[i];
      if (cell != null && QuoteHelper.hasMismatchedQuotes(quoteCharacter, cell)) {
        reportMismatchedQuote(cell);
      }
    }

    if (invalidRowsCount < invalidRowsLimit) {
      report(new InvalidRow(source_row, table_index, row, expected_length));
    }

    invalidRowsCount++;
  }

  @Override
  public ProblemSummary summarize() {
    var summary = super.summarize();
    if (invalidRowsCount > invalidRowsLimit) {
      long additionalInvalidRows = invalidRowsCount - invalidRowsLimit;
      summary.add(new AdditionalInvalidRows(additionalInvalidRows));
    }
    return summary;
  }
}
