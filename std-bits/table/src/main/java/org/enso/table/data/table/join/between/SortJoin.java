package org.enso.table.data.table.join.between;

import java.util.ArrayList;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.PluggableJoinStrategy;
import org.enso.table.data.table.join.conditions.Between;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

public class SortJoin implements JoinStrategy, PluggableJoinStrategy {

  public SortJoin(List<Between> conditions) {
    conditionsHelper = new JoinStrategy.ConditionsHelper(conditions);

    Context context = Context.getCurrent();
    int nConditions = conditions.size();
    directions = new int[nConditions];
    leftStorages = new Storage<?>[nConditions];
    lowerStorages = new Storage<?>[nConditions];
    upperStorages = new Storage<?>[nConditions];
    for (int i = 0; i < nConditions; i++) {
      directions[i] = 1;
      leftStorages[i] = conditions.get(i).left().getStorage();
      lowerStorages[i] = conditions.get(i).rightLower().getStorage();
      upperStorages[i] = conditions.get(i).rightUpper().getStorage();
      context.safepoint();
    }
  }

  private final JoinStrategy.ConditionsHelper conditionsHelper;

  private final int[] directions;
  private final Storage<?>[] leftStorages;
  private final Storage<?>[] lowerStorages;
  private final Storage<?>[] upperStorages;

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();

    int leftRowCount = conditionsHelper.getLeftTableRowCount();
    List<OrderedMultiValueKey> leftKeys = new ArrayList<>(leftRowCount);
    for (int i = 0; i < leftRowCount; i++) {
      leftKeys.add(new OrderedMultiValueKey(leftStorages, i, directions));
      context.safepoint();
    }

    NavigableSet<OrderedMultiValueKey> leftIndex = buildSortedLeftIndex(leftKeys);
    JoinResult.Builder resultBuilder = new JoinResult.Builder();

    if (leftIndex.isEmpty()) {
      return resultBuilder.build();
    }

    int rightRowCount = conditionsHelper.getRightTableRowCount();
    for (int rightRowIx = 0; rightRowIx < rightRowCount; rightRowIx++) {
      addMatchingLeftRows(leftIndex, rightRowIx, resultBuilder);
      context.safepoint();
    }

    return resultBuilder.build();
  }

  @Override
  public void joinSubsets(
      List<Integer> leftGroup,
      List<Integer> rightGroup,
      JoinResult.Builder resultBuilder,
      ProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();

    List<OrderedMultiValueKey> leftKeys =
        leftGroup.stream().map(i -> new OrderedMultiValueKey(leftStorages, i, directions)).toList();
    NavigableSet<OrderedMultiValueKey> leftIndex = buildSortedLeftIndex(leftKeys);

    if (leftIndex.isEmpty()) {
      return;
    }

    for (int rightRowIx : rightGroup) {
      addMatchingLeftRows(leftIndex, rightRowIx, resultBuilder);
      context.safepoint();
    }
  }

  private NavigableSet<OrderedMultiValueKey> buildSortedLeftIndex(List<OrderedMultiValueKey> keys) {
    Context context = Context.getCurrent();
    TreeSet<OrderedMultiValueKey> index = new TreeSet<>();
    for (var key : keys) {
      context.safepoint();
      if (key.hasAnyNulls()) {
        continue;
      }
      index.add(key);
    }
    return index;
  }

  private OrderedMultiValueKey buildLowerBound(int rightRowIx) {
    return new OrderedMultiValueKey(lowerStorages, rightRowIx, directions);
  }

  private OrderedMultiValueKey buildUpperBound(int rightRowIx) {
    return new OrderedMultiValueKey(upperStorages, rightRowIx, directions);
  }

  void addMatchingLeftRows(
      NavigableSet<OrderedMultiValueKey> sortedKeys,
      int rightRowIx,
      JoinResult.Builder resultBuilder) {
    OrderedMultiValueKey lowerBound = buildLowerBound(rightRowIx);
    OrderedMultiValueKey upperBound = buildUpperBound(rightRowIx);

    // If the match interval is invalid or empty, there is nothing to do.
    if (lowerBound.hasAnyNulls()
        || upperBound.hasAnyNulls()
        || lowerBound.compareTo(upperBound) > 0) {
      return;
    }

    NavigableSet<OrderedMultiValueKey> firstCoordinateMatches =
        sortedKeys.subSet(lowerBound, true, upperBound, true);
    ArrayList<Integer> result = new ArrayList<>();
    Context context = Context.getCurrent();
    for (OrderedMultiValueKey key : firstCoordinateMatches) {
      boolean isInRange = lowerBound.compareTo(key) <= 0 && key.compareTo(upperBound) <= 0;
      if (isInRange) {
        resultBuilder.addRow(key.getRowIndex(), rightRowIx);
      }

      context.safepoint();
    }
  }
}
