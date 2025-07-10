package org.enso.table.data.index;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.LongFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.unary.CountNothing;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;
import org.graalvm.polyglot.Context;

public class MultiValueIndex<KeyType extends MultiValueKeyBase> {
  private final ProblemAggregator problemAggregator;
  private final Column[] keyColumns;
  private final Map<KeyType, List<Long>> locs;
  private final boolean isUnique;

  public static MultiValueIndex<OrderedMultiValueKey> makeOrderedIndex(
      Column[] keyColumns,
      long tableSize,
      int[] ordering,
      Comparator<Object> objectComparator,
      ProblemAggregator problemAggregator) {
    TreeMap<OrderedMultiValueKey, List<Long>> locs = new TreeMap<>();
    final var storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
    LongFunction<OrderedMultiValueKey> keyFactory =
        i -> new OrderedMultiValueKey(storage, i, ordering, objectComparator);
    return new MultiValueIndex<>(keyColumns, tableSize, locs, keyFactory, problemAggregator);
  }

  public static MultiValueIndex<UnorderedMultiValueKey> makeUnorderedIndex(
      Column[] keyColumns,
      long tableSize,
      List<TextFoldingStrategy> textFoldingStrategies,
      ProblemAggregator problemAggregator) {
    HashMap<UnorderedMultiValueKey, List<Long>> locs = new HashMap<>();
    final var storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(ColumnStorage[]::new);
    LongFunction<UnorderedMultiValueKey> keyFactory =
        i -> new UnorderedMultiValueKey(storage, i, textFoldingStrategies);
    return new MultiValueIndex<>(keyColumns, tableSize, locs, keyFactory, problemAggregator);
  }

  public static MultiValueIndex<UnorderedMultiValueKey> makeUnorderedIndex(
      Column[] keyColumns,
      long tableSize,
      TextFoldingStrategy commonTextFoldingStrategy,
      ProblemAggregator problemAggregator) {
    var strategies = ConstantList.make(commonTextFoldingStrategy, keyColumns.length);
    return makeUnorderedIndex(keyColumns, tableSize, strategies, problemAggregator);
  }

  private MultiValueIndex(
      Column[] keyColumns,
      long tableSize,
      Map<KeyType, List<Long>> initialLocs,
      LongFunction<KeyType> keyFactory,
      ProblemAggregator problemAggregator) {
    this.keyColumns = keyColumns;
    this.locs = initialLocs;
    this.problemAggregator = problemAggregator;

    if (keyColumns.length != 0) {
      boolean isUnique = true;
      long size = keyColumns[0].getSize();
      var groupingProblemAggregator = new ColumnAggregatedProblemAggregator(problemAggregator);

      Context context = Context.getCurrent();
      for (long i = 0; i < size; i++) {
        KeyType key = keyFactory.apply(i);
        key.checkAndReportFloatingEquality(
            groupingProblemAggregator, columnIx -> keyColumns[columnIx].getName());

        var ids = this.locs.computeIfAbsent(key, x -> new ArrayList<>());
        ids.add(i);
        isUnique = isUnique && ids.size() == 1;

        context.safepoint();
      }

      this.isUnique = isUnique;
    } else {
      this.isUnique = tableSize <= 1;
      this.locs.put(
          keyFactory.apply(0), LongStream.range(0, tableSize).boxed().collect(Collectors.toList()));
    }
  }

  public boolean isUnique() {
    return isUnique;
  }

  public Table makeTable(Aggregator[] columns) {
    Context context = Context.getCurrent();
    final int length = columns.length;
    final int size = locs.size();

    boolean emptyScenario = size == 0 && keyColumns.length == 0;
    Builder[] storage =
        Arrays.stream(columns)
            .map(c -> c.makeBuilder(emptyScenario ? 1 : size, problemAggregator))
            .toArray(Builder[]::new);

    if (emptyScenario) {
      // No grouping and no data
      List<Integer> empty = new ArrayList<>();
      for (int i = 0; i < length; i++) {
        storage[i].append(columns[i].aggregate(empty, problemAggregator));
        context.safepoint();
      }
    } else {
      for (List<Long> group_locs : this.locs.values()) {
        // ToDo: Temporary workaround to avoid redoing all aggregators.
        var mapped = group_locs.stream().map(Long::intValue).collect(Collectors.toList());
        for (int i = 0; i < length; i++) {
          Object value = columns[i].aggregate(mapped, problemAggregator);
          storage[i].append(value);
          context.safepoint();
        }
      }
    }

    return new Table(
        IntStream.range(0, length)
            .mapToObj(i -> new Column(columns[i].getName(), storage[i].seal()))
            .toArray(Column[]::new));
  }

  public Set<KeyType> keys() {
    return locs.keySet();
  }

  public boolean contains(KeyType key) {
    return this.locs.containsKey(key);
  }

  public List<Long> get(KeyType key) {
    return this.locs.get(key);
  }

  public Map<KeyType, List<Long>> mapping() {
    return locs;
  }

  public int size() {
    return this.locs.size();
  }

  /**
   * Finds a key of which at least one cell is null. Returns that key, or null if no such key is
   * found.
   */
  public KeyType findAnyNullKey() {
    for (Column c : keyColumns) {
      boolean containsNulls = CountNothing.anyNothing(c.getStorage());
      if (containsNulls) {
        for (KeyType key : locs.keySet()) {
          if (key.hasAnyNulls()) {
            return key;
          }
        }

        assert false : "Null values found in a column, so a null key should be found";
      }
    }

    return null;
  }
}
