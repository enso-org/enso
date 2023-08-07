package org.enso.base.problems;

import java.util.List;

public record ProblemList<ProblemType>(List<ProblemType> problems) implements ProblemSet<ProblemType> {

  @Override
  public List<ProblemType> getProblems() {
    return problems;
  }

  @Override
  public long totalProblemCount() {
    return problems.size();
  }
}
