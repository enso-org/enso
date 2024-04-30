package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.base.polyglot.NumericConverter;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.IgnoredNothing;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

abstract class RunningGenerator {

  Column sourceColumn;
  long[] result;
  BitSet isNothing;
  ColumnAggregatedProblemAggregator columnAggregatedProblemAggregator;

  RunningGenerator(Column sourceColumn, ProblemAggregator problemAggregator) {
    this.sourceColumn = sourceColumn;
    result = new long[sourceColumn.getSize()];
    isNothing = new BitSet();
    columnAggregatedProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
  }

  void calculateNextValue(int i, RunningIterator it) {
    Object value = sourceColumn.getStorage().getItemBoxed(i);
    if (value == null) {
      columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
          new IgnoredNothing(sourceColumn.getName(), i));
    }
    Double dValue = NumericConverter.tryConvertingToDouble(value);
    Double dNextValue;
    if (dValue != null && dValue.equals(Double.NaN)) {
      columnAggregatedProblemAggregator.reportColumnAggregatedProblem(
          new IgnoredNothing(sourceColumn.getName(), i));
      dNextValue = it.currentValue();
    } else {
      dNextValue = it.next(dValue);
    }

    if (dNextValue == null) {
      isNothing.set(i);
    } else {
      result[i] = Double.doubleToRawLongBits(dNextValue);
    }
  }

  // implement this method in subclasses to control the order you want to iterate over the data
  public abstract void generate(RunningIteratorFactory factory);

  public static RunningGenerator createGenerator(
      Column sourceColumn,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    RunningGenerator runningGenerator;
    if (groupingColumns.length > 0 && orderingColumns.length > 0) {
      runningGenerator =
          new GroupingOrderingRunning(
              sourceColumn, groupingColumns, orderingColumns, directions, problemAggregator);
    } else if (groupingColumns.length > 0) {
      runningGenerator =
          new GroupingNoOrderingRunning(sourceColumn, groupingColumns, problemAggregator);
    } else if (orderingColumns.length > 0) {
      runningGenerator =
          new NoGroupingOrderingRunning(
              sourceColumn, orderingColumns, directions, problemAggregator);
    } else {
      runningGenerator = new NoGroupingNoOrderingRunning(sourceColumn, problemAggregator);
    }
    return runningGenerator;
  }
}

class NoGroupingNoOrderingRunning extends RunningGenerator {

  NoGroupingNoOrderingRunning(Column sourceColumn, ProblemAggregator problemAggregator) {
    super(sourceColumn, problemAggregator);
  }

  @Override
  public void generate(RunningIteratorFactory factory) {
    var it = factory.getIterator();
    for (int i = 0; i < result.length; i++) {
      calculateNextValue(i, it);
    }
  }
}

class GroupingNoOrderingRunning extends RunningGenerator {

  private final Column[] groupingColumns;
  private final Storage<?>[] groupingStorages;
  private final ColumnAggregatedProblemAggregator groupingProblemAggregator;
  private final List<TextFoldingStrategy> textFoldingStrategy;
  private final Map<UnorderedMultiValueKey, RunningIterator> groups;

  public GroupingNoOrderingRunning(
      Column sourceColumn, Column[] groupingColumns, ProblemAggregator problemAggregator) {
    super(sourceColumn, problemAggregator);
    this.groupingColumns = groupingColumns;
    groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);
    textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    groups = new HashMap<>();
  }

  @Override
  public void generate(RunningIteratorFactory factory) {
    for (int i = 0; i < result.length; i++) {
      var key = new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      RunningIterator it = groups.computeIfAbsent(key, k -> factory.getIterator());
      calculateNextValue(i, it);
    }
  }
}

class NoGroupingOrderingRunning extends RunningGenerator {

  private final Storage<?>[] orderingStorages;
  private final List<OrderedMultiValueKey> keys;

  public NoGroupingOrderingRunning(
      Column sourceColumn,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    super(sourceColumn, problemAggregator);
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
  public void generate(RunningIteratorFactory factory) {
    var it = factory.getIterator();
    for (var key : keys) {
      var i = key.getRowIndex();
      calculateNextValue(i, it);
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
      Column sourceColumn,
      Column[] groupingColumns,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    super(sourceColumn, problemAggregator);
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
  public void generate(RunningIteratorFactory factory) {
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
        calculateNextValue(i, it);
      }
    }
  }
}
