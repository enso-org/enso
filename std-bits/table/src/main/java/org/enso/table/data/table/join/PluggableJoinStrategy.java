package org.enso.table.data.table.join;

import java.util.List;
import org.enso.table.problems.ProblemAggregator;

/**
 * A helper join strategy that can be used within another join strategy to perform a join of
 * sub-sets of indices, stemming from already joining on other conditions.
 */
public interface PluggableJoinStrategy {

  /** Performs a join of two sub-sets of indices. */
  void joinSubsets(
      List<Integer> leftGroup,
      List<Integer> rightGroup,
      JoinResult.Builder resultBuilder,
      ProblemAggregator problemAggregator);
}
