package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

import java.util.List;

/** An aggregator for parsing problems. */
public interface ProblemAggregator {

  /**
   * Reports a cell with an invalid format.
   *
   * <p>The reports are aggregated and finally a single problem containing all invalid cells for the
   * given column is reported.
   */
  void reportInvalidFormat(String cell);

  /** Reports that a mismatched quote has been encountered. */
  void reportMismatchedQuote(String cellText);

  /**
   * Checks if there are any problems already reported.
   *
   * <p>This method returns true if and only if {@code getAggregatedProblems} would return a
   * non-empty list.
   */
  boolean hasProblems();

  /** Return an aggregated summary of problems that have been reported. */
  List<Problem> getAggregatedProblems();
}
