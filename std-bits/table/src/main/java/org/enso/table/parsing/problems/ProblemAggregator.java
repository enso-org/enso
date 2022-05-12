package org.enso.table.parsing.problems;

import java.util.List;

public interface ProblemAggregator {
  List<ParsingProblem> getAggregatedProblems();
}
