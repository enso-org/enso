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
import org.enso.table.util.ConstantList;
import org.graalvm.collections.Pair;

public class AddRowNumber {

  public static LongStorage create_grouped_numbering(
      long start, long step, Column[] groupingColumns) {
    assert groupingColumns.length > 0;
    int n = groupingColumns[0].getSize();
    long[] numbers = new long[n];
    Storage<?>[] groupingStorages =
        Arrays.stream(groupingColumns).map(Column::getStorage).toArray(Storage[]::new);
    Map<UnorderedMultiValueKey, Long> groups = new HashMap<>();
    List<TextFoldingStrategy> textFoldingStrategy =
        ConstantList.make(TextFoldingStrategy.unicodeNormalizedFold, groupingStorages.length);
    for (int i = 0; i < n; i++) {
      UnorderedMultiValueKey key =
          new UnorderedMultiValueKey(groupingStorages, i, textFoldingStrategy);
      long currentIx = groups.getOrDefault(key, start);
      numbers[i] = currentIx;
      long nextIx = Math.addExact(currentIx, step);
      groups.put(key, nextIx);
    }
    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static LongStorage create_ordered_numbering(
      long start, long step, Column[] orderingColumns, int[] directions) {
    assert orderingColumns.length > 0;
    assert orderingColumns.length == directions.length;
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

    long nextIx = start;
    for (var key : keys) {
      numbers[key.getRight()] = nextIx;
      nextIx = Math.addExact(nextIx, step);
    }
    return new LongStorage(numbers, IntegerType.INT_64);
  }

  public static LongStorage create_grouped_ordered_numbering(
      long start, long step, Column[] orderingColumns, int[] directions, Column[] groupingColumns) {
    assert orderingColumns.length > 0;
    assert orderingColumns.length == directions.length;

    int n = orderingColumns[0].getSize();
    Storage<?>[] orderingStorages =
        Arrays.stream(orderingColumns).map(Column::getStorage).toArray(Storage[]::new);
    long[] numbers = new long[n];
    MultiValueIndex<UnorderedMultiValueKey> groupIndex =
        MultiValueIndex.makeUnorderedIndex(
            groupingColumns, n, TextFoldingStrategy.unicodeNormalizedFold);

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
      long nextIx = start;
      for (var key : orderingKeys) {
        numbers[key.getRight()] = nextIx;
        nextIx = Math.addExact(nextIx, step);
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
}
