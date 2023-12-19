package org.enso.table.data.table.join.hashing;

import java.util.List;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.PluggableJoinStrategy;
import org.enso.table.data.table.join.conditions.Equals;
import org.enso.table.data.table.join.conditions.EqualsIgnoreCase;
import org.enso.table.data.table.join.conditions.HashableCondition;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/**
 * A strategy that uses a hash-map to perform join on the equality conditions.
 *
 * <p>It then delegates to {@code remainingMatcher} to perform the remaining conditions on the
 * matching pairs of row subsets.
 */
public class HashJoin implements JoinStrategy {
  public HashJoin(
      List<HashableCondition> conditions,
      PluggableJoinStrategy remainingMatcher,
      JoinResult.BuilderSettings resultBuilderSettings) {
    conditionsHelper = new JoinStrategy.ConditionsHelper(conditions);
    this.remainingMatcher = remainingMatcher;
    this.resultBuilderSettings = resultBuilderSettings;

    List<HashEqualityCondition> equalConditions =
        conditions.stream().map(HashJoin::makeHashEqualityCondition).toList();

    if (equalConditions.isEmpty()) {
      throw new IllegalArgumentException(
          "EqualityHashJoin is applicable if there is at least one equality condition.");
    }

    leftEquals = equalConditions.stream().map(HashEqualityCondition::left).toArray(Column[]::new);
    rightEquals = equalConditions.stream().map(HashEqualityCondition::right).toArray(Column[]::new);
    textFoldingStrategies =
        equalConditions.stream().map(HashEqualityCondition::textFoldingStrategy).toList();
  }

  private final JoinStrategy.ConditionsHelper conditionsHelper;
  private final Column[] leftEquals, rightEquals;
  private final List<TextFoldingStrategy> textFoldingStrategies;
  private final PluggableJoinStrategy remainingMatcher;
  private final JoinResult.BuilderSettings resultBuilderSettings;

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();

    var leftIndex =
        MultiValueIndex.makeUnorderedIndex(
            leftEquals,
            conditionsHelper.getLeftTableRowCount(),
            textFoldingStrategies,
            problemAggregator);
    var rightIndex =
        MultiValueIndex.makeUnorderedIndex(
            rightEquals,
            conditionsHelper.getRightTableRowCount(),
            textFoldingStrategies,
            problemAggregator);

    JoinResult.Builder resultBuilder = new JoinResult.Builder(resultBuilderSettings);
    for (var leftEntry : leftIndex.mapping().entrySet()) {
      UnorderedMultiValueKey leftKey = leftEntry.getKey();
      List<Integer> leftRows = leftEntry.getValue();
      List<Integer> rightRows = rightIndex.get(leftKey);

      if (rightRows != null) {
        remainingMatcher.joinSubsets(leftRows, rightRows, resultBuilder, problemAggregator);
      } else {
        if (resultBuilderSettings.wantsLeftUnmatched()) {
          for (int leftRow : leftRows) {
            resultBuilder.addUnmatchedLeftRow(leftRow);
            context.safepoint();
          }
        }
      }

      context.safepoint();
    }

    if (resultBuilderSettings.wantsRightUnmatched()) {
      for (var rightEntry : rightIndex.mapping().entrySet()) {
        UnorderedMultiValueKey rightKey = rightEntry.getKey();
        boolean wasCompletelyUnmatched = !leftIndex.contains(rightKey);
        if (wasCompletelyUnmatched) {
          for (int rightRow : rightEntry.getValue()) {
            resultBuilder.addUnmatchedRightRow(rightRow);
          }
        }
      }
    }

    return resultBuilder.build();
  }

  private static HashEqualityCondition makeHashEqualityCondition(HashableCondition eq) {
    switch (eq) {
      case Equals e -> {
        return new HashEqualityCondition(
            e.left(), e.right(), TextFoldingStrategy.unicodeNormalizedFold);
      }
      case EqualsIgnoreCase e -> {
        return new HashEqualityCondition(
            e.left(), e.right(), TextFoldingStrategy.caseInsensitiveFold(e.locale()));
      }
    }
  }

  private record HashEqualityCondition(
      Column left, Column right, TextFoldingStrategy textFoldingStrategy) {}
}
