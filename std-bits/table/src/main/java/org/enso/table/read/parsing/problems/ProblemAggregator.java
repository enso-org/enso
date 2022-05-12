package org.enso.table.read.parsing.problems;

import java.util.List;

public interface ProblemAggregator {
  List<ParsingProblem> getAggregatedProblems();
}
