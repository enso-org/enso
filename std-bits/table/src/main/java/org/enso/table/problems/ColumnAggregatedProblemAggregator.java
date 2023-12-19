package org.enso.table.problems;

import java.util.ArrayList;
import java.util.List;
import org.enso.table.data.table.problems.ColumnAggregatedProblem;

/** A ProblemAggregator that is capable of merging ColumnAggregatedProblems. */
public class ColumnAggregatedProblemAggregator extends ProblemAggregator {
  public ColumnAggregatedProblemAggregator(ProblemAggregator parent) {
    super(parent);
  }

  private final List<ColumnAggregatedProblem> aggregatedProblemList = new ArrayList<>();

  public void reportColumnAggregatedProblem(ColumnAggregatedProblem problem) {
    for (ColumnAggregatedProblem p : aggregatedProblemList) {
      if (p.merge(problem)) {
        // The problem was merged with an existing one.
        return;
      }
    }

    aggregatedProblemList.add(problem);
  }

  @Override
  public ProblemSummary summarize() {
    var summary = super.summarize();
    for (var p : aggregatedProblemList) {
      summary.add(p);
    }
    return summary;
  }
}
