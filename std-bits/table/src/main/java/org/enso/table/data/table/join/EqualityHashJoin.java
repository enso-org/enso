package org.enso.table.data.table.join;

import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.List;

/**
 * A strategy that uses a hash-map to perform join on the equality conditions.
 * <p>
 * It then delegates to {@code remainingMatcher} to perform the remaining conditions on the matching pairs of row
 * subsets.
 */
public class EqualityHashJoin implements JoinStrategy {
  public EqualityHashJoin(List<HashableCondition> conditions, PluggableJoinStrategy remainingMatcher) {
    conditionsHelper = new JoinStrategy.ConditionsHelper(conditions);
    this.remainingMatcher = remainingMatcher;

    List<HashEqualityCondition> equalConditions =
        conditions.stream().filter(EqualityHashJoin::isSupported).map(EqualityHashJoin::makeHashEqualityCondition).toList();

    if (equalConditions.isEmpty()) {
      throw new IllegalArgumentException("EqualityHashJoin is applicable if there is at least one equality condition.");
    }

    leftEquals = equalConditions.stream().map(HashEqualityCondition::left).toArray(Column[]::new);
    rightEquals = equalConditions.stream().map(HashEqualityCondition::right).toArray(Column[]::new);
    textFoldingStrategies = equalConditions.stream().map(HashEqualityCondition::textFoldingStrategy).toList();
  }

  private final JoinStrategy.ConditionsHelper conditionsHelper;
  private final Column[] leftEquals, rightEquals;
  private final List<TextFoldingStrategy> textFoldingStrategies;
  private final PluggableJoinStrategy remainingMatcher;

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();

    var leftIndex = MultiValueIndex.makeUnorderedIndex(leftEquals, conditionsHelper.getLeftTableRowCount(),
        textFoldingStrategies, problemAggregator);
    var rightIndex = MultiValueIndex.makeUnorderedIndex(rightEquals, conditionsHelper.getRightTableRowCount(),
        textFoldingStrategies, problemAggregator);

    JoinResult.Builder resultBuilder = new JoinResult.Builder();
    for (var leftEntry : leftIndex.mapping().entrySet()) {
      UnorderedMultiValueKey leftKey = leftEntry.getKey();
      List<Integer> leftRows = leftEntry.getValue();
      List<Integer> rightRows = rightIndex.get(leftKey);

      if (rightRows != null) {
        remainingMatcher.joinSubsets(leftRows, rightRows, resultBuilder, problemAggregator);
      }

      context.safepoint();
    }

    return resultBuilder.build();
  }

  private static boolean isSupported(JoinCondition condition) {
    return switch (condition) {
      case Equals ignored -> true;
      case EqualsIgnoreCase ignored -> true;
      default -> false;
    };
  }

  private static HashEqualityCondition makeHashEqualityCondition(HashableCondition eq) {
    switch (eq) {
      case Equals e -> {
        return new HashEqualityCondition(e.left(), e.right(), TextFoldingStrategy.unicodeNormalizedFold);
      }
      case EqualsIgnoreCase e -> {
        return new HashEqualityCondition(e.left(), e.right(), TextFoldingStrategy.caseInsensitiveFold(e.locale()));
      }
      default ->
          throw new IllegalStateException("Impossible: trying to convert condition " + eq + " to a " +
              "HashEqualityCondition, but it should not be marked as supported. This is a" + " bug in the Table " +
              "library.");
    }
  }

  private record HashEqualityCondition(Column left, Column right, TextFoldingStrategy textFoldingStrategy) {
  }
}
