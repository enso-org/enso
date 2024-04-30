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

abstract class RunningGenerator {

  // implement this method in subclasses to control the order you want to iterate over the data
  public abstract void generate(RunningIteratorFactory factory, long numRows);

  public static RunningGenerator createGenerator(
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    RunningGenerator runningGenerator;
    if (groupingColumns.length > 0 && orderingColumns.length > 0) {
      runningGenerator =
          new GroupingOrderingRunning(
              groupingColumns, orderingColumns, directions, problemAggregator);
    } else if (groupingColumns.length > 0) {
      runningGenerator = new GroupingNoOrderingRunning(groupingColumns, problemAggregator);
    } else if (orderingColumns.length > 0) {
      runningGenerator = new NoGroupingOrderingRunning(orderingColumns, directions);
    } else {
      runningGenerator = new NoGroupingNoOrderingRunning();
    }
    return runningGenerator;
  }
}

class NoGroupingNoOrderingRunning extends RunningGenerator {

  NoGroupingNoOrderingRunning() {}

  @Override
  public void generate(RunningIteratorFactory factory, long numRows) {
    var it = factory.getIterator();
    for (int i = 0; i < numRows; i++) {
      factory.calculateNextValue(i, it);
    }
  }
}

class GroupingNoOrderingRunning extends RunningGenerator {

  private final Column[] groupingColumns;
  private final Storage<?>[] groupingStorages;
  private final ColumnAggregatedProblemAggregator groupingProblemAggregator;
  private final List<TextFoldingStrategy> textFoldingStrategy;
  private final Map<UnorderedMultiValueKey, RunningIterator> groups;

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
  public void generate(RunningIteratorFactory factory, long numRows) {
    for (int i = 0; i < numRows; i++) {
      var key = new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      RunningIterator it = groups.computeIfAbsent(key, k -> factory.getIterator());
      factory.calculateNextValue(i, it);
    }
  }
}

class NoGroupingOrderingRunning extends RunningGenerator {

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
  public void generate(RunningIteratorFactory factory, long numRows) {
    var it = factory.getIterator();
    for (var key : keys) {
      var i = key.getRowIndex();
      factory.calculateNextValue(i, it);
    }
  }
}

class GroupingOrderingRunning extends RunningGenerator {

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
  public void generate(RunningIteratorFactory factory, long numRows) {
    int n = orderingColumns[0].getSize();
    var groupIndex =
        MultiValueIndex.makeUnorderedIndex(
            groupingColumns, n, TextFoldingStrategy.unicodeNormalizedFold, problemAggregator);
    for (var entry : groupIndex.mapping().entrySet()) {
      List<Integer> indices = entry.getValue();
      List<OrderedMultiValueKey> orderingKeys =
          new ArrayList<>(
              indices.stream()
                  .map(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                  .toList());
      orderingKeys.sort(null);
      RunningIterator it = factory.getIterator();
      for (OrderedMultiValueKey key : orderingKeys) {
        var i = key.getRowIndex();
        factory.calculateNextValue(i, it);
      }
    }
  }
}
