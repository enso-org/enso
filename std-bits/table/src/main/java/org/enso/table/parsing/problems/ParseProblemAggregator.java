package org.enso.table.parsing.problems;

import org.enso.table.problems.ProblemAggregator;

/**
 * An aggregator for parsing problems.
 */
public sealed interface ParseProblemAggregator permits ParseProblemAggregatorImpl, NoOpParseProblemAggregator, SimplifiedParseProblemAggregator {

  /**
   * Reports a cell with an invalid format.
   *
   * <p>The reports are aggregated and finally a single problem containing all invalid cells for the
   * given column is reported.
   */
  void reportInvalidFormat(String cell);

  /**
   * Reports that a mismatched quote has been encountered.
   */
  void reportMismatchedQuote(String cellText);

  /**
   * Checks if there are any problems already reported.
   */
  boolean hasProblems();

  /** The preferred way to construct a ParseProblemAggregator. */
  static ParseProblemAggregatorImpl make(ProblemAggregator parent, String columnName, Object expectedEnsoValueType) {
    return new ParseProblemAggregatorImpl(parent, columnName, expectedEnsoValueType);
  }
}
