package org.enso.table.parsing.problems;

import java.util.ArrayList;
import java.util.List;

/**
 * An aggregator for parsing problems.
 *
 * <p>Each strategy exposes a method that returns a summary of the problems. The particular methods
 * for reporting each problem are defined in particular subclasses.
 */
public class ProblemAggregator {

  private final List<String> invalidFormatCells = new ArrayList<>();
  private final List<String> leadingZerosCells = new ArrayList<>();
  private int mismatchedQuotes = 0;
  private final String relatedColumnName;

  public ProblemAggregator(String relatedColumnName) {
    this.relatedColumnName = relatedColumnName;
  }

  /**
   * Reports a cell with an invalid format.
   *
   * <p>The reports are aggregated and finally a single problem containing all invalid cell for the
   * given column is reported.
   */
  public void reportInvalidFormat(String cell) {
    invalidFormatCells.add(cell);
  }

  public void reportLeadingZeroes(String cell) {
    leadingZerosCells.add(cell);
  }

  public void reportMismatchedQuote() {
    mismatchedQuotes++;
  }

  /**
   * Checks if there are any problems already reported.
   *
   * <p>This method returns true if and only if {@code getAggregatedProblems} would return a
   * non-empty list.
   */
  public boolean hasProblems() {
    return !invalidFormatCells.isEmpty() || !leadingZerosCells.isEmpty() || mismatchedQuotes > 0;
  }

  /** Return an aggregated summary of problems that have been reported. */
  public List<ParsingProblem> getAggregatedProblems() {
    List<ParsingProblem> problems = new ArrayList<>();

    if (!invalidFormatCells.isEmpty()) {
      problems.add(new InvalidFormat(relatedColumnName, invalidFormatCells));
    }

    if (!leadingZerosCells.isEmpty()) {
      problems.add(new LeadingZeros(relatedColumnName, leadingZerosCells));
    }

    for (int i = 0; i < mismatchedQuotes; ++i) {
      problems.add(new MismatchedQuote());
    }

    assert problems.isEmpty() == !hasProblems();

    return problems;
  }
}
