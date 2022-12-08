package org.enso.table.data.index;

import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.object.*;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.FloatingPointGrouping;

import java.util.*;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class MultiValueIndex {
  private final int keyColumnsLength;
  private final Map<MultiValueKeyBase, List<Integer>> locs;
  private final AggregatedProblems problems;
  private final Comparator<Object> comparator;

  public MultiValueIndex(Column[] keyColumns, int tableSize, Comparator<Object> objectComparator) {
    this(keyColumns, tableSize, null, objectComparator);
  }

  public MultiValueIndex(
      Column[] keyColumns, int tableSize, int[] ordering, Comparator<Object> objectComparator) {
    this.keyColumnsLength = keyColumns.length;
    this.comparator = objectComparator;
    this.problems = new AggregatedProblems();

    boolean isOrdered = ordering != null;
    this.locs = isOrdered ? new TreeMap<>() : new HashMap<>();

    Storage<?>[] storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
    IntFunction<MultiValueKeyBase> keyFactory =
        isOrdered
            ? i -> new OrderedMultiValueKey(storage, i, ordering, objectComparator)
            : i -> new UnorderedMultiValueKey(storage, i);

    if (keyColumns.length != 0) {
      int size = keyColumns[0].getSize();

      for (int i = 0; i < size; i++) {
        MultiValueKeyBase key = keyFactory.apply(i);

        if (key.hasFloatValues()) {
          problems.add(new FloatingPointGrouping("GroupBy", i));
        }

        List<Integer> ids = this.locs.computeIfAbsent(key, x -> new ArrayList<>());
        ids.add(i);
      }
    } else {
      this.locs.put(
          keyFactory.apply(0), IntStream.range(0, tableSize).boxed().collect(Collectors.toList()));
    }
  }

  public Table makeTable(Aggregator[] columns) {
    final int length = columns.length;
    final int size = locs.size();

    boolean emptyScenario = size == 0 & keyColumnsLength == 0;
    Builder[] storage =
        Arrays.stream(columns)
            .map(c -> Builder.getForType(c.getType(), emptyScenario ? 1 : size))
            .toArray(Builder[]::new);

    if (emptyScenario) {
      // No grouping and no data
      List<Integer> empty = new ArrayList<>();
      for (int i = 0; i < length; i++) {
        storage[i].appendNoGrow(columns[i].aggregate(empty));
      }
    } else {
      for (List<Integer> group_locs : this.locs.values()) {
        for (int i = 0; i < length; i++) {
          Object value = columns[i].aggregate(group_locs);
          storage[i].appendNoGrow(value);
        }
      }
    }

    // Merge Problems
    AggregatedProblems[] problems = new AggregatedProblems[1 + length];
    problems[0] = this.problems;
    IntStream.range(0, length).forEach(i -> problems[i + 1] = columns[i].getProblems());
    AggregatedProblems merged = AggregatedProblems.merge(problems);

    return new Table(
        IntStream.range(0, length)
            .mapToObj(i -> new Column(columns[i].getName(), storage[i].seal()))
            .toArray(Column[]::new),
        merged);
  }

  public Table makeCrossTabTable(
      Column[] groupingColumns, Column nameColumn, Aggregator[] aggregates) {
    final int size = locs.size();

    var nameIndex =
        new MultiValueIndex(new Column[] {nameColumn}, nameColumn.getSize(), null, this.comparator);
    final int columnCount = groupingColumns.length + nameIndex.locs.size() * aggregates.length;

    // Create the storage
    Builder[] storage = new Builder[columnCount];
    IntStream.range(0, groupingColumns.length)
        .forEach(
            i -> storage[i] = Builder.getForType(groupingColumns[i].getStorage().getType(), size));
    IntStream.range(0, nameIndex.locs.size())
        .forEach(
            i -> {
              int offset = groupingColumns.length + i * aggregates.length;
              IntStream.range(0, aggregates.length)
                  .forEach(
                      j -> storage[offset + j] = Builder.getForType(aggregates[j].getType(), size));
            });

    // Fill the storage
    for (List<Integer> group_locs : this.locs.values()) {
      // Fill the grouping columns
      IntStream.range(0, groupingColumns.length)
          .forEach(
              i ->
                  storage[i].appendNoGrow(
                      groupingColumns[i].getStorage().getItemBoxed(group_locs.get(0))));

      // Make a Set
      var groupSet = new HashSet<>(group_locs);

      // Fill the aggregates
      int offset = groupingColumns.length;
      for (List<Integer> name_locs : nameIndex.locs.values()) {
        var filtered = name_locs.stream().filter(groupSet::contains).collect(Collectors.toList());

        for (int i = 0; i < aggregates.length; i++) {
          storage[offset + i].appendNoGrow(aggregates[i].aggregate(filtered));
        }

        offset += aggregates.length;
      }
    }

    // Merge Problems
    AggregatedProblems[] problems = new AggregatedProblems[aggregates.length + 1];
    problems[0] = this.problems;
    IntStream.range(0, aggregates.length)
        .forEach(i -> problems[i + 1] = aggregates[i].getProblems());
    AggregatedProblems merged = AggregatedProblems.merge(problems);

    // Create Columns
    Column[] output = new Column[columnCount];
    IntStream.range(0, groupingColumns.length)
        .forEach(i -> output[i] = new Column(groupingColumns[i].getName(), storage[i].seal()));

    int offset = groupingColumns.length;
    for (List<Integer> name_locs : nameIndex.locs.values()) {
      String name = nameColumn.getStorage().getItemBoxed(name_locs.get(0)).toString();

      for (int i = 0; i < aggregates.length; i++) {
        output[offset + i] =
            new Column((name + " " + aggregates[i].getName()).trim(), storage[offset + i].seal());
      }

      offset += aggregates.length;
    }

    return new Table(output, merged);
  }

  public int[] makeOrderMap(int rowCount) {
    if (this.locs.size() == 0) {
      return new int[0];
    }

    int[] output = new int[rowCount];

    int idx = 0;
    for (List<Integer> rowIndexes : this.locs.values()) {
      for (Integer rowIndex : rowIndexes) {
        output[idx++] = rowIndex;
      }
    }

    return output;
  }

  public Set<MultiValueKeyBase> keys() {
    return locs.keySet();
  }

  public boolean contains(MultiValueKeyBase key) {
    return this.locs.containsKey(key);
  }

  public List<Integer> get(MultiValueKeyBase key) {
    return this.locs.get(key);
  }
}
