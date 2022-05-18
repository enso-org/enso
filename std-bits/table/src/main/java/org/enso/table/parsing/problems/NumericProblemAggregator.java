package org.enso.table.parsing.problems;

import java.util.ArrayList;
import java.util.List;

/**
 * A problem aggregator capable of reporting {@code InvalidFormat} and {@code LeadingZeros}
 * problems.
 */
public class NumericProblemAggregator extends InvalidFormatProblemAggregator {
  private final List<String> leadingZerosCells = new ArrayList<>();

  public void reportLeadingZeroes(String cell) {
    leadingZerosCells.add(cell);
  }

  @Override
  public List<ParsingProblem> getAggregatedProblems() {
    List<ParsingProblem> problems = new ArrayList<>(super.getAggregatedProblems());

    if (!leadingZerosCells.isEmpty()) {
      problems.add(new LeadingZeros(leadingZerosCells));
    }

    return problems;
  }
}
