package org.enso.table.read.parsing.problems;

import java.util.ArrayList;
import java.util.List;

public class NumericProblemAggregator extends InvalidFormatProblemAggregator {
  private List<String> leadingZerosCells;

  public void reportLeadingZeroes(String cell) {
    leadingZerosCells.add(cell);
  }

  @Override
  public List<ParsingProblem> getAggregatedProblems() {
    List<ParsingProblem> problems = new ArrayList<>(super.getAggregatedProblems());

    if (!leadingZerosCells.isEmpty()) {
      problems.add(new LeadingZeroes(leadingZerosCells));
    }

    return problems;
  }
}
