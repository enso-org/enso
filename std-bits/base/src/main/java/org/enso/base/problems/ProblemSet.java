package org.enso.base.problems;

import java.util.List;

public interface ProblemSet<ProblemType> {
  List<ProblemType> getProblems();
  long totalProblemCount();
}
