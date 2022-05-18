package org.enso.table.parsing.problems;

import java.util.ArrayList;
import java.util.List;

/**
 * A base problem aggregator that allows reporting the most generic {@code InvalidFormat} problem.
 */
public class InvalidFormatProblemAggregator implements ProblemAggregator {

  private final List<String> invalidFormatCells = new ArrayList<>();

  /**
   * Reports a cell with an invalid format.
   *
   * <p>The reports are aggregated and finally a single problem containing all invalid cell for the
   * given column is reported.
   */
  public void reportInvalidFormat(String cell) {
    invalidFormatCells.add(cell);
  }

  @Override
  public List<ParsingProblem> getAggregatedProblems() {
    if (invalidFormatCells.isEmpty()) return List.of();
    else return List.of(new InvalidFormat(invalidFormatCells));
  }
}
