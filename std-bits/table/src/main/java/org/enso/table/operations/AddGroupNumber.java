package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.index.OrderedMultiValueKey;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;

public class AddGroupNumber {
  public static Storage<?> numberGroupsUnique(
      long numRows,
      long start,
      long step,
      Column[] groupingColumns,
      ProblemAggregator problemAggregator) {
    if (groupingColumns.length == 0) {
      throw new IllegalArgumentException("At least one grouping column is required.");
    }

    var groupNumberIterator = new StepIterator(start, step);

    long[] numbers = new long[Math.toIntExact(numRows)];

    Storage<?>[] groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    ColumnAggregatedProblemAggregator groupingProblemAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    List<TextFoldingStrategy> textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    Map<UnorderedMultiValueKey, Long> groupNumbers = new HashMap<>();

    for (int i = 0; i < numRows; i++) {
      var key = new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      var groupNumber = groupNumbers.computeIfAbsent(key, k -> groupNumberIterator.next());
      numbers[i] = groupNumber;
    }

    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static Storage<?> numberGroupsEqualCount(
      long numRows,
      int groupCount,
      long start,
      long step,
      Column[] orderingColumns,
      int[] directions,
      ProblemAggregator problemAggregator) {
    long[] numbers = new long[Math.toIntExact(numRows)];

    var equalCountGenerator = new EqualCountGenerator(start, step, numRows, groupCount);

    if (orderingColumns.length == 0) {
      for (int i = 0; i < numRows; ++i) {
        numbers[i] = equalCountGenerator.next();
      }
    } else {
      Storage<?>[] orderingStorages =
          Arrays.stream(orderingColumns).map(Column::getStorage).toArray(Storage[]::new);
      List<OrderedMultiValueKey> keys =
          new ArrayList<>(
              IntStream.range(0, Math.toIntExact(numRows))
                  .mapToObj(i -> new OrderedMultiValueKey(orderingStorages, i, directions))
                  .toList());
      keys.sort(null);
      for (var key : keys) {
        var i = key.getRowIndex();
        numbers[i] = equalCountGenerator.next();
      }
    }

    return new LongStorage(numbers, IntegerType.INT_64);
  }

  private static class StepIterator {
    private final long step;
    private long current;

    public StepIterator(long start, long step) {
      this.step = step;
      this.current = start;
    }

    public long next() {
      var toReturn = current;
      current = Math.addExact(current, step);
      return toReturn;
    }
  }

  private static class EqualCountGenerator {
    private final long start;
    private final long step;
    private long currentIndex = 0;
    private final long groupSize;

    public EqualCountGenerator(long start, long step, long totalCount, long numgroups) {
      this.start = start;
      this.step = step;
      groupSize = (long) Math.ceil((double) totalCount / (double) numgroups);
    }

    public long next() {
      long toReturn = Math.addExact(start, Math.multiplyExact(step, (currentIndex / groupSize)));
      currentIndex = Math.addExact(currentIndex, 1L);
      return toReturn;
    }
  }
}
