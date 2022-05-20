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

  /**
   * Checks if there are any problems already reported.
   *
   * <p>This method returns true if and only if {@code getAggregatedProblems} would return a
   * non-empty list.
   */
  public boolean hasProblems() {
    return !invalidFormatCells.isEmpty() || !leadingZerosCells.isEmpty();
  }

  /** Return an aggregated summary of problems that have been reported. */
  public List<ParsingProblem> getAggregatedProblems() {
    List<ParsingProblem> problems = new ArrayList<>();

    if (!invalidFormatCells.isEmpty()) {
      problems.add(new InvalidFormat(invalidFormatCells));
    }

    if (!leadingZerosCells.isEmpty()) {
      problems.add(new LeadingZeros(leadingZerosCells));
    }

    assert problems.isEmpty() == !hasProblems();

    return problems;
  }
}
