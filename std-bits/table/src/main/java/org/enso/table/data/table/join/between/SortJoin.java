package org.enso.table.data.table.join.between;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.enso.base.ObjectComparator;
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
    JoinResult.Builder resultBuilder = new JoinResult.Builder();

    int leftRowCount = conditionsHelper.getLeftTableRowCount();
    int rightRowCount = conditionsHelper.getRightTableRowCount();
    if (leftRowCount == 0 || rightRowCount == 0) {
      // if one group is completely empty, there will be no matches to report
      return resultBuilder.build();
    }
    List<OrderedMultiValueKey> leftKeys = new ArrayList<>(leftRowCount);
    for (int i = 0; i < leftRowCount; i++) {
      leftKeys.add(new OrderedMultiValueKey(leftStorages, i, directions));
      context.safepoint();
    }

    SortedListIndex<OrderedMultiValueKey> leftIndex = buildSortedLeftIndex(leftKeys);

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
        leftGroup.stream()
            .map(i -> new OrderedMultiValueKey(leftStorages, i, directions, objectComparator))
            .toList();
    if (leftKeys.isEmpty()) {
      // left group is completely empty - there will be no matches at all
      return;
    }

    SortedListIndex<OrderedMultiValueKey> leftIndex = buildSortedLeftIndex(leftKeys);

    for (int rightRowIx : rightGroup) {
      addMatchingLeftRows(leftIndex, rightRowIx, resultBuilder);
      context.safepoint();
    }
  }

  private SortedListIndex<OrderedMultiValueKey> buildSortedLeftIndex(
      List<OrderedMultiValueKey> keys) {
    return SortedListIndex.build(keys, firstCoordinateComparator);
  }

  private OrderedMultiValueKey buildLowerBound(int rightRowIx) {
    return new OrderedMultiValueKey(lowerStorages, rightRowIx, directions, objectComparator);
  }

  private OrderedMultiValueKey buildUpperBound(int rightRowIx) {
    return new OrderedMultiValueKey(upperStorages, rightRowIx, directions, objectComparator);
  }

  private void addMatchingLeftRows(
      SortedListIndex<OrderedMultiValueKey> sortedLeftIndex,
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

    List<OrderedMultiValueKey> firstCoordinateMatches =
        sortedLeftIndex.findSubRange(lowerBound, upperBound);
    Context context = Context.getCurrent();
    for (OrderedMultiValueKey key : firstCoordinateMatches) {
      if (isInRange(key, lowerBound, upperBound)) {
        resultBuilder.addRow(key.getRowIndex(), rightRowIx);
      }

      context.safepoint();
    }
  }

  private boolean isInRange(
      OrderedMultiValueKey key, OrderedMultiValueKey lowerBound, OrderedMultiValueKey upperBound) {
    assert key.getNumberOfColumns() == lowerBound.getNumberOfColumns();
    assert key.getNumberOfColumns() == upperBound.getNumberOfColumns();

    // Note: we cannot just use `compareTo`, because we are now not checking that the key is between
    // the bounds in lexicographic order.
    // Instead, we are checking if the key is between the bounds for all dimensions.

    int n = key.getNumberOfColumns();
    for (int i = 0; i < n; i++) {
      var keyValue = key.get(i);
      var lowerBoundValue = lowerBound.get(i);
      var upperBoundValue = upperBound.get(i);
      boolean fitsInThisDimension =
          objectComparator.compare(keyValue, lowerBoundValue) >= 0
              && objectComparator.compare(keyValue, upperBoundValue) <= 0;
      if (!fitsInThisDimension) {
        return false;
      }
    }

    return true;
  }

  private final ObjectComparator objectComparator = ObjectComparator.DEFAULT;
  private final Comparator<OrderedMultiValueKey> firstCoordinateComparator =
      new OrderedMultiValueKey.ProjectionComparator(0);
}
