package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

abstract class RunningLooper<T> {

  // implement this method in subclasses to control the order you want to loop over the data
  public abstract void loopImpl(RunningStatistic<T> runningStatistic, long numRows);

  public static <T> void loop(
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator,
      RunningStatistic<T> runningStatistic,
      long numRows) {
    RunningLooper<T> runningLooper;
    if (groupingColumns.length > 0 && orderingColumns.length > 0) {
      runningLooper =
          new GroupingOrderingRunning<>(
              groupingColumns, orderingColumns, directions, problemAggregator);
    } else if (groupingColumns.length > 0) {
      runningLooper = new GroupingNoOrderingRunning<>(groupingColumns, problemAggregator);
    } else if (orderingColumns.length > 0) {
      runningLooper = new NoGroupingOrderingRunning<>(orderingColumns, directions);
    } else {
      runningLooper = new NoGroupingNoOrderingRunning<>();
    }
    runningLooper.loopImpl(runningStatistic, numRows);
  }
}

class NoGroupingNoOrderingRunning<T> extends RunningLooper<T> {

  NoGroupingNoOrderingRunning() {}

  @Override
  public void loopImpl(RunningStatistic<T> runningStatistic, long numRows) {
    var it = runningStatistic.getNewIterator();
    for (int i = 0; i < numRows; i++) {
      runningStatistic.calculateNextValue(i, it);
    }
  }
}

class GroupingNoOrderingRunning<T> extends RunningLooper<T> {

  private final Column[] groupingColumns;
  private final Storage<?>[] groupingStorages;
  private final ColumnAggregatedProblemAggregator groupingProblemAggregator;
  private final List<TextFoldingStrategy> textFoldingStrategy;
  private final Map<UnorderedMultiValueKey, RunningIterator<T>> groups;

  public GroupingNoOrderingRunning(Column[] groupingColumns, ProblemAggregator problemAggregator) {
    this.groupingColumns = groupingColumns;
    groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    groups = new HashMap<>();
  }

  @Override
  public void loopImpl(RunningStatistic<T> runningStatistic, long numRows) {
    for (int i = 0; i < numRows; i++) {
      var key = new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      var it = groups.computeIfAbsent(key, k -> runningStatistic.getNewIterator());
      runningStatistic.calculateNextValue(i, it);
    }
  }
}

class NoGroupingOrderingRunning<T> extends RunningLooper<T> {

  private final Storage<?>[] orderingStorages;
  private final List<OrderedMultiValueKey> keys;

  public NoGroupingOrderingRunning(Column[] orderingColumns, int[] directions) {
    int n = orderingColumns[0].getSize();
    orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(Storage[]::new);
    keys =
        new ArrayList<>(
            IntStream.range(0, n)
                .mapToObj(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                .toList());
    keys.sort(null);
  }

  @Override
  public void loopImpl(RunningStatistic<T> runningStatistic, long numRows) {
    var it = runningStatistic.getNewIterator();
    for (var key : keys) {
      var i = key.getRowIndex();
      runningStatistic.calculateNextValue(i, it);
    }
  }
}

class GroupingOrderingRunning<T> extends RunningLooper<T> {

  private final Column[] groupingColumns;
  private final Column[] orderingColumns;
  private final int[] directions;
  private final Storage<?>[] groupingStorages;
  private final Storage<?>[] orderingStorages;
  private final ProblemAggregator problemAggregator;

  public GroupingOrderingRunning(
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    this.groupingColumns = groupingColumns;
    this.orderingColumns = orderingColumns;
    this.directions = directions;
    groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(Storage[]::new);
    this.problemAggregator = problemAggregator;
  }

  @Override
  public void loopImpl(RunningStatistic<T> runningStatistic, long numRows) {
    var groupIndex =
        MultiValueIndex.makeUnorderedIndex(
            groupingColumns,
            (int) numRows,
            TextFoldingStrategy.unicodeNormalizedFold,
            problemAggregator);
    for (var entry : groupIndex.mapping().entrySet()) {
      List<Integer> indices = entry.getValue();
      List<OrderedMultiValueKey> orderingKeys =
          new ArrayList<>(
              indices.stream()
                  .map(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                  .toList());
      orderingKeys.sort(null);
      var it = runningStatistic.getNewIterator();
      for (OrderedMultiValueKey key : orderingKeys) {
        var i = key.getRowIndex();
        runningStatistic.calculateNextValue(i, it);
      }
    }
  }
}
