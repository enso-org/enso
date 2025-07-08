package org.enso.table.data.table.join.between;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.enso.base.ObjectComparator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.join.JoinKind;
import org.enso.table.data.table.join.JoinResult;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.conditions.Between;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

public class SortJoin implements JoinStrategy {

  public SortJoin(List<Between> conditions, JoinKind joinKind) {
    JoinStrategy.ensureConditionsNotEmpty(conditions);
    this.joinKind = joinKind;

    Context context = Context.getCurrent();
    int nConditions = conditions.size();
    directions = new int[nConditions];
    leftStorages = new ColumnStorage<?>[nConditions];
    lowerStorages = new ColumnStorage<?>[nConditions];
    upperStorages = new ColumnStorage<?>[nConditions];
    for (int i = 0; i < nConditions; i++) {
      directions[i] = 1;
      leftStorages[i] = conditions.get(i).left().getStorage();
      lowerStorages[i] = conditions.get(i).rightLower().getStorage();
      upperStorages[i] = conditions.get(i).rightUpper().getStorage();
      context.safepoint();
    }
  }

  private final JoinKind joinKind;

  private final int[] directions;
  private final ColumnStorage<?>[] leftStorages;
  private final ColumnStorage<?>[] lowerStorages;
  private final ColumnStorage<?>[] upperStorages;
  private final Set<Long> matchedLeftRows = new HashSet<>();

  @Override
  public JoinResult join(ProblemAggregator problemAggregator) {
    var resultBuilder = new JoinResult.Builder();

    long leftRowCount = leftStorages[0].getSize();
    long rightRowCount = lowerStorages[0].getSize();
    if (leftRowCount == 0 || rightRowCount == 0) {
      // if one group is completely empty, there will be no matches to report
      return resultBuilder.buildAndInvalidate();
    }

    Context context = Context.getCurrent();
    List<OrderedMultiValueKey> leftKeys = new ArrayList<>(Builder.checkSize(leftRowCount));
    for (long i = 0; i < leftRowCount; i++) {
      leftKeys.add(new OrderedMultiValueKey(leftStorages, i, directions));
      context.safepoint();
    }

    SortedListIndex<OrderedMultiValueKey> leftIndex = buildSortedLeftIndex(leftKeys);

    for (long rightRowIx = 0; rightRowIx < rightRowCount; rightRowIx++) {
      long matches = addMatchingLeftRows(leftIndex, rightRowIx, resultBuilder);
      if (joinKind.wantsRightUnmatched && matches == 0) {
        resultBuilder.addUnmatchedRightRow(rightRowIx);
      }
      context.safepoint();
    }

    if (joinKind.wantsLeftUnmatched) {
      for (long leftRowIx = 0; leftRowIx < leftRowCount; leftRowIx++) {
        if (!matchedLeftRows.contains(leftRowIx)) {
          resultBuilder.addUnmatchedLeftRow(leftRowIx);
        }
        context.safepoint();
      }
    }

    return resultBuilder.buildAndInvalidate();
  }

  public void joinSubsets(
      List<Long> leftGroup,
      List<Long> rightGroup,
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

    for (long rightRowIx : rightGroup) {
      long matches = addMatchingLeftRows(leftIndex, rightRowIx, resultBuilder);
      if (joinKind.wantsRightUnmatched && matches == 0) {
        resultBuilder.addUnmatchedRightRow(rightRowIx);
      }
      context.safepoint();
    }

    if (joinKind.wantsLeftUnmatched) {
      for (long leftRowIx : leftGroup) {
        if (!matchedLeftRows.contains(leftRowIx)) {
          resultBuilder.addUnmatchedLeftRow(leftRowIx);
        }
        context.safepoint();
      }
    }
  }

  private SortedListIndex<OrderedMultiValueKey> buildSortedLeftIndex(
      List<OrderedMultiValueKey> keys) {
    return SortedListIndex.build(keys, firstCoordinateComparator);
  }

  private OrderedMultiValueKey buildLowerBound(long rightRowIx) {
    return new OrderedMultiValueKey(lowerStorages, rightRowIx, directions, objectComparator);
  }

  private OrderedMultiValueKey buildUpperBound(long rightRowIx) {
    return new OrderedMultiValueKey(upperStorages, rightRowIx, directions, objectComparator);
  }

  /**
   * Adds all pairs of rows from the left index matching the right index to the builder, and reports
   * the match count.
   *
   * <p>It also marks any of the left rows that were matched, in the {@code matchedLeftRows}.
   */
  private long addMatchingLeftRows(
      SortedListIndex<OrderedMultiValueKey> sortedLeftIndex,
      long rightRowIx,
      JoinResult.Builder resultBuilder) {
    OrderedMultiValueKey lowerBound = buildLowerBound(rightRowIx);
    OrderedMultiValueKey upperBound = buildUpperBound(rightRowIx);

    // If the match interval is invalid or empty, there is nothing to do.
    if (lowerBound.hasAnyNulls()
        || upperBound.hasAnyNulls()
        || lowerBound.compareTo(upperBound) > 0) {
      return 0;
    }

    long matchCount = 0;

    List<OrderedMultiValueKey> firstCoordinateMatches =
        sortedLeftIndex.findSubRange(lowerBound, upperBound);
    Context context = Context.getCurrent();
    for (OrderedMultiValueKey key : firstCoordinateMatches) {
      if (isInRange(key, lowerBound, upperBound)) {
        long leftRowIx = key.getRowIndex();
        matchCount++;
        if (joinKind.wantsCommon) {
          resultBuilder.addMatchedRowsPair(leftRowIx, rightRowIx);
        }
        if (joinKind.wantsLeftUnmatched) {
          matchedLeftRows.add(leftRowIx);
        }
      }

      context.safepoint();
    }

    return matchCount;
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
