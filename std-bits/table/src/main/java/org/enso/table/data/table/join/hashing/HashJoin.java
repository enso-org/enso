package org.enso.table.data.table.join.hashing;

import java.util.List;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.join.JoinKind;
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
      JoinKind joinKind) {
    JoinStrategy.ensureConditionsNotEmpty(conditions);
    this.remainingMatcher = remainingMatcher;
    this.joinKind = joinKind;

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

  private final Column[] leftEquals, rightEquals;
  private final List<TextFoldingStrategy> textFoldingStrategies;
  private final PluggableJoinStrategy remainingMatcher;
  private final JoinKind joinKind;

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();

    var leftIndex =
        MultiValueIndex.makeUnorderedIndex(
            leftEquals, leftEquals[0].getSize(), textFoldingStrategies, problemAggregator);
    var rightIndex =
        MultiValueIndex.makeUnorderedIndex(
            rightEquals, rightEquals[0].getSize(), textFoldingStrategies, problemAggregator);

    JoinResult.Builder resultBuilder = new JoinResult.Builder();
    for (var leftEntry : leftIndex.mapping().entrySet()) {
      UnorderedMultiValueKey leftKey = leftEntry.getKey();
      List<Integer> leftRows = leftEntry.getValue();
      // If any field of the key is null, it cannot match anything.
      List<Integer> rightRows = leftKey.hasAnyNulls() ? null : rightIndex.get(leftKey);

      if (rightRows != null) {
        remainingMatcher.joinSubsets(leftRows, rightRows, resultBuilder, problemAggregator);
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
