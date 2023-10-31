package org.enso.table.data.table.join;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class SortJoin implements PluggableJoinStrategy {
  @Override
  public JoinResult join(Table left, Table right, List<JoinCondition> conditions, ProblemAggregator problemAggregator) {
    assert !conditions.isEmpty();
    List<Between> rangeConditions = conditions.stream().map(c -> {
      if (c instanceof Between b) {
        return b;
      } else {
        throw new IllegalArgumentException("Only Between conditions are supported for SortJoin.");
      }
    }).toList();

    SortedLeftIndex leftIndex = buildSortedLeftIndex(rangeConditions);
    JoinResult.Builder resultBuilder = new JoinResult.Builder();

    Context context = Context.getCurrent();
    int rightRowCount = right.rowCount();
    for (int rightRowIx = 0; rightRowIx < rightRowCount; rightRowIx++) {
      List<Integer> leftRows = leftIndex.findMatchingLeftRows(rightRowIx);
      for (int leftRowIx : leftRows) {
        resultBuilder.addRow(leftRowIx, rightRowIx);

        context.safepoint();
      }

      context.safepoint();
    }

    return resultBuilder.build();
  }

  private SortedLeftIndex buildSortedLeftIndex(List<Between> conditions) {
    Context context = Context.getCurrent();

    assert !conditions.isEmpty();
    int leftRowCount = conditions.get(0).left().getStorage().size();
    int[] directions = new int[conditions.size()];
    Storage<?>[] storages = new Storage<?>[conditions.size()];
    for (int i = 0; i < conditions.size(); i++) {
      directions[i] = 1;
      storages[i] = conditions.get(i).left().getStorage();
      context.safepoint();
    }

    List<OrderedMultiValueKey> keys = new ArrayList<>(leftRowCount);
    for (int i = 0; i < leftRowCount; i++) {
      keys.add(new OrderedMultiValueKey(storages, i, directions));
      context.safepoint();
    }

    keys.sort(compareOnlyFirstKey);
    return new SortedLeftIndex(keys, conditions.get(0), conditions.subList(1, conditions.size()));
  }

  final class SortedLeftIndex {
    private final List<OrderedMultiValueKey> sortedKeys;
    private final Between sortCondition;
    private final List<Between> remainingConditions;

    private final int[] directions;
    private final Storage<?>[] lowerStorages;
    private final Storage<?>[] upperStorages;

    SortedLeftIndex(List<OrderedMultiValueKey> sortedKeys, Between sortCondition, List<Between> remainingConditions) {
      this.sortedKeys = sortedKeys;
      this.sortCondition = sortCondition;
      this.remainingConditions = remainingConditions;
    }

    List<Integer> findMatchingLeftRows(int rightRowIx) {
      OrderedMultiValueKey lowerBound = buildLowerBound(rightRowIx);
      OrderedMultiValueKey upperBound = buildUpperBound(rightRowIx);

      int leftMostIx = findLeftBoundGreaterOrEqual(lowerBound);
    }

    private OrderedMultiValueKey buildLowerBound(int rightRowIx) {
      return new OrderedMultiValueKey(lowerStorages, rightRowIx, directions);
    }

    private OrderedMultiValueKey buildUpperBound(int rightRowIx) {
      return new OrderedMultiValueKey(upperStorages, rightRowIx, directions);
    }

    private int findLeftBoundGreaterOrEqual(OrderedMultiValueKey lower) {
      int loc = Collections.binarySearch(sortedKeys, lower, compareOnlyFirstKey);
      if (loc >= 0) {
        // We have found _any_ object equal to the lower bound, but we need to find the first one.
        Context context = Context.getCurrent();
        while (loc > 0 && compareOnlyFirstKey.compare(sortedKeys.get(loc - 1), lower) == 0) {
          loc--;
          context.safepoint();
        }

        return loc;
      } else {
        int insertionPoint = -(loc + 1);
      }
    }

  }

  private Comparator<OrderedMultiValueKey> compareOnlyFirstKey = new OrderedMultiValueKey.LimitedIndexComparator(1);
}
