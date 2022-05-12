package org.enso.table.read.parsing.problems;

import java.util.ArrayList;
import java.util.List;

public class InvalidFormatProblemAggregator implements ProblemAggregator {

  private List<String> invalidFormatCells = new ArrayList<>();

  public void reportInvalidFormat(String cell) {
    invalidFormatCells.add(cell);
  }

  @Override
  public List<ParsingProblem> getAggregatedProblems() {
    if (invalidFormatCells.isEmpty()) return List.of();
    else return List.of(new InvalidFormat(invalidFormatCells));
  }
}
