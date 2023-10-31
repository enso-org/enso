package org.enso.table.data.table.join;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.NavigableSet;
import java.util.TreeSet;

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

    TreeSet<OrderedMultiValueKey> keys = new TreeSet<>();
    for (int i = 0; i < leftRowCount; i++) {
      keys.add(new OrderedMultiValueKey(storages, i, directions));
      context.safepoint();
    }

    return new SortedLeftIndex(keys, conditions.get(0), conditions.subList(1, conditions.size()));
  }

  static final class SortedLeftIndex {
    private final NavigableSet<OrderedMultiValueKey> sortedKeys;

    private final int[] directions;
    private final Storage<?>[] lowerStorages;
    private final Storage<?>[] upperStorages;

    SortedLeftIndex(NavigableSet<OrderedMultiValueKey> sortedKeys, Between sortCondition,
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

      NavigableSet<OrderedMultiValueKey> firstCoordinateMatches = sortedKeys.subSet(lowerBound, true, upperBound, true);
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

    private OrderedMultiValueKey buildLowerBound(int rightRowIx) {
      return new OrderedMultiValueKey(lowerStorages, rightRowIx, directions);
    }

    private OrderedMultiValueKey buildUpperBound(int rightRowIx) {
      return new OrderedMultiValueKey(upperStorages, rightRowIx, directions);
    }
  }
}
