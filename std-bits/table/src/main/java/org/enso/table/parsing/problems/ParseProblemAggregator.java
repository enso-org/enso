package org.enso.table.parsing.problems;

import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;

/**
 * A simple interface for aggregating parsing problems.
 * <p>
 * It is separate from the {@link ProblemAggregator} mechanism, because in some cases when parsing we may not care about
 * particular content of the problems, but only want to know if a problem has occurred. Thus, we allow for a simplified
 * implementation {@link ShortCircuitParseProblemAggregator} that only tracks if a problem has occurred. The default
 * implementation, {@link CommonParseProblemAggregator} will tie in to the main {@link ProblemAggregator} mechanism.
 */
public sealed interface ParseProblemAggregator permits CommonParseProblemAggregator, NoOpParseProblemAggregator,
    ShortCircuitParseProblemAggregator {

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

  /**
   * The preferred way to construct a ParseProblemAggregator.
   */
  static CommonParseProblemAggregator make(ProblemAggregator parent, String columnName, Value expectedEnsoValueType) {
    return new CommonParseProblemAggregator(parent, columnName, expectedEnsoValueType);
  }
}
