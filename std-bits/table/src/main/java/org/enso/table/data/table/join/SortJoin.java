package org.enso.table.data.table.join;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.ArrayList;
import java.util.Collections;
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

    ArrayList<OrderedMultiValueKey> keys = new ArrayList<>();
    for (int i = 0; i < leftRowCount; i++) {
      keys.add(new OrderedMultiValueKey(storages, i, directions));
      context.safepoint();
    }
    keys.sort(null);

    return new SortedLeftIndex(keys, conditions.get(0), conditions.subList(1, conditions.size()));
  }

  final class SortedLeftIndex {
    private final List<OrderedMultiValueKey> sortedKeys;

    private final int[] directions;
    private final Storage<?>[] lowerStorages;
    private final Storage<?>[] upperStorages;

    SortedLeftIndex(ArrayList<OrderedMultiValueKey> sortedKeys, Between sortCondition,
                    List<Between> remainingConditions) {
      this.sortedKeys = sortedKeys;
      int nConditions = 1 + remainingConditions.size();
      directions = new int[nConditions];
      lowerStorages = new Storage<?>[nConditions];
      upperStorages = new Storage<?>[nConditions];
      for (int i = 0; i < nConditions; i++) {
        directions[i] = 1;
        if (i == 0) {
          lowerStorages[i] = sortCondition.rightLower().getStorage();
          upperStorages[i] = sortCondition.rightUpper().getStorage();
        } else {
          Between condition = remainingConditions.get(i - 1);
          lowerStorages[i] = condition.rightLower().getStorage();
          upperStorages[i] = condition.rightUpper().getStorage();
        }
      }
    }

    List<Integer> findMatchingLeftRows(int rightRowIx) {
      if (sortedKeys.isEmpty()) {
        return Collections.emptyList();
      }

      OrderedMultiValueKey lowerBound = buildLowerBound(rightRowIx);
      OrderedMultiValueKey upperBound = buildUpperBound(rightRowIx);

      if (lowerBound.compareTo(upperBound) > 0) {
        return Collections.emptyList();
      }

      List<OrderedMultiValueKey> firstCoordinateMatches = findFirstCoordinateMatches(lowerBound, upperBound);
      ArrayList<Integer> result = new ArrayList<>();
      Context context = Context.getCurrent();
      for (OrderedMultiValueKey key : firstCoordinateMatches) {
        boolean isInRange = lowerBound.compareTo(key) <= 0 && key.compareTo(lowerBound) <= 0;
        if (isInRange) {
          result.add(key.getRowIndex());
        }

        context.safepoint();
      }

      return result;
    }

    private List<OrderedMultiValueKey> findFirstCoordinateMatches(OrderedMultiValueKey lowerBound,
                                                                  OrderedMultiValueKey upperBound) {
      int lowerBoundIx = findLowerBoundIx(lowerBound);
      int upperBoundIx = findUpperBoundIx(upperBound);
      return sortedKeys.subList(lowerBoundIx, upperBoundIx);
    }

    private int findLowerBoundIx(OrderedMultiValueKey lowerBound) {
      int ix = Collections.binarySearch(sortedKeys, lowerBound);
      if (ix >= 0) {
        while (ix > 0 && sortedKeys.get(ix - 1).compareTo(lowerBound) == 0) {
          ix--;
        }

        return ix;
      } else {
        return -ix - 1;
      }
    }

    private int findUpperBoundIx(OrderedMultiValueKey upperBound) {
      int ix = Collections.binarySearch(sortedKeys, upperBound);
      if (ix >= 0) {
        while (ix < sortedKeys.size() - 1 && sortedKeys.get(ix + 1).compareTo(upperBound) == 0) {
          ix++;
        }

        return ix + 1;
      } else {
        return -ix - 1;
      }
    }

    private OrderedMultiValueKey buildLowerBound(int rightRowIx) {
      return new OrderedMultiValueKey(lowerStorages, rightRowIx, directions);
    }

    private OrderedMultiValueKey buildUpperBound(int rightRowIx) {
      return new OrderedMultiValueKey(upperStorages, rightRowIx, directions);
    }
  }
}
