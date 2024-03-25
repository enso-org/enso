package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.index.MultiValueIndex;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

public class AddRunning {

  public static Storage<?> create_grouped_running(
      Column sourceColumn, Column[] groupingColumns, ProblemAggregator problemAggregator) {
    if (groupingColumns.length == 0) {
      throw new IllegalArgumentException("At least one grouping column is required.");
    }

    int n = sourceColumn.getSize();
    long[] numbers = new long[n];
    Storage<?>[] groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    ColumnAggregatedProblemAggregator groupingProblemAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    List<TextFoldingStrategy> textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    Map<UnorderedMultiValueKey, RunningIterator> groups = new HashMap<>();
    Storage<?> sourceStorage = sourceColumn.getStorage();
    for (int i = 0; i < n; i++) {
      UnorderedMultiValueKey key =
          new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      RunningIterator it = groups.computeIfAbsent(key, k -> new RunningIterator());
      numbers[i] = it.next(sourceStorage.getItemBoxed(i));
    }
    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static Storage<?> create_ordered_running(
      Column sourceColumn, Column[] orderingColumns, int[] directions) {
    if (orderingColumns.length == 0) {
      throw new IllegalArgumentException("At least one ordering column is required.");
    }
    if (orderingColumns.length != directions.length) {
      throw new IllegalArgumentException(
          "The number of ordering columns and directions must be the same.");
    }

    int n = orderingColumns[0].getSize();
    Storage<?>[] orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(Storage[]::new);
    long[] numbers = new long[n];
    List<OrderedMultiValueKey> keys =
        new ArrayList<>(
            IntStream.range(0, n)
                .mapToObj(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                .toList());

    keys.sort(null);

    RunningIterator it = new RunningIterator();
    Storage<?> sourceStorage = sourceColumn.getStorage();
    for (var key : keys) {
      var i = key.getRowIndex();
      numbers[i] = it.next(sourceStorage.getItemBoxed(i));
    }
    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static Storage<?> create_grouped_ordered_running(
      Column sourceColumn,
      Column[] orderingColumns,
      int[] directions,
      Column[] groupingColumns,
      ProblemAggregator problemAggregator) {
    if (orderingColumns.length == 0) {
      throw new IllegalArgumentException("At least one ordering column is required.");
    }
    if (orderingColumns.length != directions.length) {
      throw new IllegalArgumentException(
          "The number of ordering columns and directions must be the same.");
    }

    int n = orderingColumns[0].getSize();
    Storage<?>[] orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(Storage[]::new);
    long[] numbers = new long[n];
    MultiValueIndex<UnorderedMultiValueKey> groupIndex =
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
      RunningIterator it = new RunningIterator();
      Storage<?> sourceStorage = sourceColumn.getStorage();
      for (OrderedMultiValueKey key : orderingKeys) {
        var i = key.getRowIndex();
        numbers[i] = it.next(sourceStorage.getItemBoxed(i));
      }
    }

    return new LongStorage(numbers, IntegerType.INT_64);
  }

  /**
   * A helper for computing consecutive numbers based on a start and step. It will throw an {@link
   * java.lang.ArithmeticException} if the next number overflows.
   */
  private static class RunningIterator {
    private Object current;

    RunningIterator() {
    }

    Object next(Object value) {
      if (current == null) {
        current = value;
      } else {
        Long lCurrent = NumericConverter.tryConvertingToLong(current);
        Long lValue = NumericConverter.tryConvertingToLong(value);
        if (lCurrent != null && lValue != null) {
          try {
            current = Math.addExact(lCurrent, lValue);
          } catch (ArithmeticException exception) {
            innerAggregator.reportOverflow(IntegerType.INT_64, "Sum");
            return null;
          }
        } else {
          Double dCurrent = NumericConverter.tryConvertingToDouble(current);
          Double dValue = NumericConverter.tryConvertingToDouble(value);
          if (dCurrent != null && dValue != null) {
            current = dCurrent + dValue;
          } else {
            innerAggregator.reportColumnAggregatedProblem(
                new InvalidAggregation(this.getName(), row, "Cannot convert to a number."));
            return null;
          }
        }
      }
      return current;
    }
  }
}
