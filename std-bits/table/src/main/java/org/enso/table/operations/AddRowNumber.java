package org.enso.table.operations;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
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
import org.graalvm.collections.Pair;

public class AddRowNumber {

  public static LongStorage create_grouped_numbering(
      long start, long step, Column[] groupingColumns, ProblemAggregator problemAggregator) {
    if (groupingColumns.length == 0) {
      throw new IllegalArgumentException("At least one grouping column is required.");
    }

    int n = groupingColumns[0].getSize();
    long[] numbers = new long[n];
    Storage<?>[] groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    ColumnAggregatedProblemAggregator groupingProblemAggregator =
        new ColumnAggregatedProblemAggregator(problemAggregator);
    List<TextFoldingStrategy> textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    Map<UnorderedMultiValueKey, RangeIterator> groups = new HashMap<>();
    for (int i = 0; i < n; i++) {
      UnorderedMultiValueKey key =
          new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      key.checkAndReportFloatingEquality(
          groupingProblemAggregator, columnIx -> groupingColumns[columnIx].getName());
      RangeIterator it = groups.computeIfAbsent(key, k -> new RangeIterator(start, step));
      numbers[i] = it.next();
    }
    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static LongStorage create_ordered_numbering(
      long start, long step, Column[] orderingColumns, int[] directions) {
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
    List<Pair<OrderedMultiValueKey, Integer>> keys =
        new ArrayList<>(
            IntStream.range(0, n)
                .mapToObj(
                    i -> Pair.create(new OrderedMultiValueKey(orderingStorages, i, directions), i))
                .toList());

    keys.sort(OrderedPairComparator.INSTANCE);

    RangeIterator it = new RangeIterator(start, step);
    for (var key : keys) {
      numbers[key.getRight()] = it.next();
    }
    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static LongStorage create_grouped_ordered_numbering(
      long start,
      long step,
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
      List<Pair<OrderedMultiValueKey, Integer>> orderingKeys =
          new ArrayList<>(
              indices.stream()
                  .map(
                      i ->
                          Pair.create(new OrderedMultiValueKey(orderingStorages, i, directions), i))
                  .toList());
      orderingKeys.sort(OrderedPairComparator.INSTANCE);
      RangeIterator it = new RangeIterator(start, step);
      for (var key : orderingKeys) {
        numbers[key.getRight()] = it.next();
      }
    }

    return new LongStorage(numbers, IntegerType.INT_64);
  }

  private static class OrderedPairComparator
      implements Comparator<Pair<OrderedMultiValueKey, Integer>> {
    @Override
    public int compare(
        Pair<OrderedMultiValueKey, Integer> o1, Pair<OrderedMultiValueKey, Integer> o2) {
      int p1 = o1.getLeft().compareTo(o2.getLeft());
      if (p1 != 0) {
        return p1;
      }

      return o1.getRight().compareTo(o2.getRight());
    }

    @Override
    public boolean equals(Object obj) {
      return obj instanceof OrderedPairComparator;
    }

    static OrderedPairComparator INSTANCE = new OrderedPairComparator();
  }

  /**
   * A helper for computing consecutive numbers based on a start and step. It will throw an {@link
   * java.lang.ArithmeticException} if the next number overflows.
   */
  private static class RangeIterator {
    private final long start;
    private final long step;
    private long current;
    private boolean isFirst = true;

    RangeIterator(long start, long step) {
      this.start = start;
      this.step = step;
    }

    long next() throws ArithmeticException {
      if (isFirst) {
        isFirst = false;
        current = start;
      } else {
        current = Math.addExact(current, step);
      }

      return current;
    }
  }
}
