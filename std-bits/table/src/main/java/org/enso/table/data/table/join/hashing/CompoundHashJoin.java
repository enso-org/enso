package org.enso.table.data.table.join.hashing;

import java.util.List;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.join.JoinKind;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.between.SortJoin;
import org.enso.table.data.table.join.conditions.Between;
import org.enso.table.data.table.join.conditions.HashableCondition;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * A strategy that uses a hash-map to perform join on the equality conditions.
 *
 * <p>It then delegates to {@code SortJoin} to perform the remaining conditions on the matching
 * pairs of row subsets.
 */
public class CompoundHashJoin implements JoinStrategy {

  public CompoundHashJoin(
      List<HashableCondition> hashableConditions,
      List<Between> betweenConditions,
      JoinKind joinKind) {
    this.hashJoinConfig = new HashJoinConfig(hashableConditions);
    this.sortJoin = new SortJoin(betweenConditions, joinKind);
    this.joinKind = joinKind;
  }

  private final HashJoinConfig hashJoinConfig;
  private final SortJoin sortJoin;
  private final JoinKind joinKind;

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();

    var leftIndex =
        MultiValueIndex.makeUnorderedIndex(
            hashJoinConfig.getLeftEquals(),
            hashJoinConfig.getLeftNumRows(),
            hashJoinConfig.getTextFoldingStrategies(),
            problemAggregator);
    var rightIndex =
        MultiValueIndex.makeUnorderedIndex(
            hashJoinConfig.getRightEquals(),
            hashJoinConfig.getRightNumRows(),
            hashJoinConfig.getTextFoldingStrategies(),
            problemAggregator);

    JoinResult.Builder resultBuilder = new JoinResult.Builder();
    for (var leftEntry : leftIndex.mapping().entrySet()) {
      UnorderedMultiValueKey leftKey = leftEntry.getKey();
      List<Integer> leftRows = leftEntry.getValue();
      // If any field of the key is null, it cannot match anything.
      List<Integer> rightRows = leftKey.hasAnyNulls() ? null : rightIndex.get(leftKey);

      if (rightRows != null) {
        sortJoin.joinSubsets(leftRows, rightRows, resultBuilder, problemAggregator);
      } else {
        if (joinKind.wantsLeftUnmatched) {
          for (int leftRow : leftRows) {
            resultBuilder.addUnmatchedLeftRow(leftRow);
            context.safepoint();
          }
        }
      }

      context.safepoint();
    }

    if (joinKind.wantsRightUnmatched) {
      for (var rightEntry : rightIndex.mapping().entrySet()) {
        UnorderedMultiValueKey rightKey = rightEntry.getKey();
        // If any field of the key is null, it cannot match anything.
        boolean wasCompletelyUnmatched =
            rightKey.hasAnyNulls() ? true : !leftIndex.contains(rightKey);
        if (wasCompletelyUnmatched) {
          for (int rightRow : rightEntry.getValue()) {
            resultBuilder.addUnmatchedRightRow(rightRow);
          }
        }
      }
    }

    return resultBuilder.buildAndInvalidate();
  }
}
