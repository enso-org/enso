package org.enso.table.data.index;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.ConstantList;
import org.graalvm.polyglot.Context;

public class MultiValueIndex<KeyType extends MultiValueKeyBase> {
  private final ProblemAggregator problemAggregator;
  private final Column[] keyColumns;
  private final Map<KeyType, List<Integer>> locs;
  private final boolean isUnique;

  public static MultiValueIndex<OrderedMultiValueKey> makeOrderedIndex(
      Column[] keyColumns,
      int tableSize,
      int[] ordering,
      Comparator<Object> objectComparator,
      ProblemAggregator problemAggregator) {
    TreeMap<OrderedMultiValueKey, List<Integer>> locs = new TreeMap<>();
    final Storage<?>[] storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
    IntFunction<OrderedMultiValueKey> keyFactory =
        i -> new OrderedMultiValueKey(storage, i, ordering, objectComparator);
    return new MultiValueIndex<>(keyColumns, tableSize, locs, keyFactory, problemAggregator);
  }

  public static MultiValueIndex<UnorderedMultiValueKey> makeUnorderedIndex(
      Column[] keyColumns,
      int tableSize,
      List<TextFoldingStrategy> textFoldingStrategies,
      ProblemAggregator problemAggregator) {
    HashMap<UnorderedMultiValueKey, List<Integer>> locs = new HashMap<>();
    final Storage<?>[] storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
    IntFunction<UnorderedMultiValueKey> keyFactory =
        i -> new UnorderedMultiValueKey(storage, i, textFoldingStrategies);
    return new MultiValueIndex<>(keyColumns, tableSize, locs, keyFactory, problemAggregator);
  }

  public static MultiValueIndex<UnorderedMultiValueKey> makeUnorderedIndex(
      Column[] keyColumns,
      int tableSize,
      TextFoldingStrategy commonTextFoldingStrategy,
      ProblemAggregator problemAggregator) {
    List<TextFoldingStrategy> strategies =
        ConstantList.make(commonTextFoldingStrategy, keyColumns.length);
    return makeUnorderedIndex(keyColumns, tableSize, strategies, problemAggregator);
  }

  private MultiValueIndex(
      Column[] keyColumns,
      int tableSize,
      Map<KeyType, List<Integer>> initialLocs,
      IntFunction<KeyType> keyFactory,
      ProblemAggregator problemAggregator) {
    this.keyColumns = keyColumns;
    this.locs = initialLocs;
    this.problemAggregator = problemAggregator;

    if (keyColumns.length != 0) {
      boolean isUnique = true;
      int size = keyColumns[0].getSize();
      ColumnAggregatedProblemAggregator groupingProblemAggregator =
          new ColumnAggregatedProblemAggregator(problemAggregator);

      Context context = Context.getCurrent();
      for (int i = 0; i < size; i++) {
        KeyType key = keyFactory.apply(i);
        key.checkAndReportFloatingEquality(
            groupingProblemAggregator, columnIx -> keyColumns[columnIx].getName());

        List<Integer> ids = this.locs.computeIfAbsent(key, x -> new ArrayList<>());
        ids.add(i);
        isUnique = isUnique && ids.size() == 1;

        context.safepoint();
      }

      this.isUnique = isUnique;
    } else {
      this.isUnique = tableSize <= 1;
      this.locs.put(
          keyFactory.apply(0), IntStream.range(0, tableSize).boxed().collect(Collectors.toList()));
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
            .map(c -> Builder.getForType(c.getType(), emptyScenario ? 1 : size, problemAggregator))
            .toArray(Builder[]::new);

    if (emptyScenario) {
      // No grouping and no data
      List<Integer> empty = new ArrayList<>();
      for (int i = 0; i < length; i++) {
        storage[i].appendNoGrow(columns[i].aggregate(empty, problemAggregator));
        context.safepoint();
      }
    } else {
      for (List<Integer> group_locs : this.locs.values()) {
        for (int i = 0; i < length; i++) {
          Object value = columns[i].aggregate(group_locs, problemAggregator);
          storage[i].appendNoGrow(value);
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

  public List<Integer> get(KeyType key) {
    return this.locs.get(key);
  }

  public Map<KeyType, List<Integer>> mapping() {
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
      boolean containsNulls = c.getStorage().countMissing() > 0;
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
