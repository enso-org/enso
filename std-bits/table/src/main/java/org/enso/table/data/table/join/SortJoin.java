package org.enso.table.data.table.join;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

import java.util.ArrayList;
import java.util.List;

public class SortJoin implements PluggableJoinStrategy {
  @Override
  public JoinResult join(Table left, Table right, List<JoinCondition> conditions, ProblemAggregator problemAggregator) {
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
      List<Integer> leftRows = leftIndex.finMatchingLeftRows(rightRowIx);
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

    keys.sort(null);
    return new SortedLeftIndex(keys, conditions);
  }

  record SortedLeftIndex(List<OrderedMultiValueKey> keys, List<Between> conditions) {
    List<Integer> findMatchingLeftRows(int rightRowIx) {
      // Currently we do a binary search on the first condition, and linear search on all subsequent ones.
      // TODO
      return null;
    }
  }

}
