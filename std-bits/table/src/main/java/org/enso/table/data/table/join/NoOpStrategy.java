package org.enso.table.data.table.join;

import java.util.List;
import org.enso.table.problems.ProblemAggregator;

public class NoOpStrategy implements PluggableJoinStrategy {
  @Override
  public void joinSubsets(
      List<Integer> leftGroup,
      List<Integer> rightGroup,
      JoinResult.Builder resultBuilder,
      ProblemAggregator problemAggregator) {
    return;
  }
}
