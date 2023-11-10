package org.enso.table.data.table.join;

import java.util.List;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * A pluggable strategy that can be used as the inner strategy for a join if there are no more join
 * conditions to process - so all rows are matched with each other within a given group.
 */
public class MatchAllStrategy implements PluggableJoinStrategy {
  @Override
  public void joinSubsets(
      List<Integer> leftGroup,
      List<Integer> rightGroup,
      JoinResult.Builder resultBuilder,
      ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    for (var leftRow : leftGroup) {
      for (var rightRow : rightGroup) {
        resultBuilder.addRow(leftRow, rightRow);
        context.safepoint();
      }

      context.safepoint();
    }
  }
}
