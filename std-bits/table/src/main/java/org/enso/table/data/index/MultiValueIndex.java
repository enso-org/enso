package org.enso.table.data.index;

import java.util.*;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.util.ConstantList;
import org.enso.table.util.NameDeduplicator;

public class MultiValueIndex<KeyType extends MultiValueKeyBase> {
  private final int keyColumnsLength;
  private final Map<KeyType, List<Integer>> locs;
  private final AggregatedProblems problems;

  public static MultiValueIndex<OrderedMultiValueKey> makeOrderedIndex(
      Column[] keyColumns, int tableSize, int[] ordering, Comparator<Object> objectComparator) {
    TreeMap<OrderedMultiValueKey, List<Integer>> locs = new TreeMap<>();
    final Storage<?>[] storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
    IntFunction<OrderedMultiValueKey> keyFactory =
        i -> new OrderedMultiValueKey(storage, i, ordering, objectComparator);
    return new MultiValueIndex<>(keyColumns, tableSize, locs, keyFactory);
  }

  public static MultiValueIndex<UnorderedMultiValueKey> makeUnorderedIndex(
      Column[] keyColumns, int tableSize, List<TextFoldingStrategy> textFoldingStrategies) {
    HashMap<UnorderedMultiValueKey, List<Integer>> locs = new HashMap<>();
    final Storage<?>[] storage =
        Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
    IntFunction<UnorderedMultiValueKey> keyFactory =
        i -> new UnorderedMultiValueKey(storage, i, textFoldingStrategies);
    return new MultiValueIndex<>(keyColumns, tableSize, locs, keyFactory);
  }

  public static MultiValueIndex<UnorderedMultiValueKey> makeUnorderedIndex(
      Column[] keyColumns, int tableSize, TextFoldingStrategy commonTextFoldingStrategy) {
    List<TextFoldingStrategy> strategies =
        ConstantList.make(commonTextFoldingStrategy, keyColumns.length);
    return makeUnorderedIndex(keyColumns, tableSize, strategies);
  }

  private MultiValueIndex(
      Column[] keyColumns,
      int tableSize,
      Map<KeyType, List<Integer>> initialLocs,
      IntFunction<KeyType> keyFactory) {
    this.keyColumnsLength = keyColumns.length;
    this.problems = new AggregatedProblems();
    this.locs = initialLocs;

    if (keyColumns.length != 0) {
      int size = keyColumns[0].getSize();

      for (int i = 0; i < size; i++) {
        KeyType key = keyFactory.apply(i);

        if (key.hasFloatValues()) {
          final int row = i;
          key.floatColumnPositions()
              .forEach(
                  columnIx ->
                      problems.add(new FloatingPointGrouping(keyColumns[columnIx].getName(), row)));
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

    boolean emptyScenario = size == 0 && keyColumnsLength == 0;
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
      Column[] groupingColumns,
      Column nameColumn,
      Aggregator[] aggregates,
      String[] aggregateNames) {
    NameDeduplicator outputTableNameDeduplicator = new NameDeduplicator();

    final int size = locs.size();

    var nameIndex =
        MultiValueIndex.makeUnorderedIndex(
            new Column[] {nameColumn},
            nameColumn.getSize(),
            TextFoldingStrategy.unicodeNormalizedFold);
    final int columnCount = groupingColumns.length + nameIndex.locs.size() * aggregates.length;

    // Create the storage
    Builder[] storage = new Builder[columnCount];
    for (int i = 0; i < groupingColumns.length; i++) {
      storage[i] = Builder.getForType(groupingColumns[i].getStorage().getType(), size);
    }

    for (int i = 0; i < nameIndex.locs.size(); i++) {
      int offset = groupingColumns.length + i * aggregates.length;
      for (int j = 0; j < aggregates.length; j++) {
        storage[offset + j] = Builder.getForType(aggregates[j].getType(), size);
      }
    }

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

    // Create Columns
    Column[] output = new Column[columnCount];
    for (int i = 0; i < groupingColumns.length; i++) {
      outputTableNameDeduplicator.markUsed(groupingColumns[i].getName());
      output[i] = new Column(groupingColumns[i].getName(), storage[i].seal());
    }

    int offset = groupingColumns.length;
    for (List<Integer> name_locs : nameIndex.locs.values()) {
      Object boxed = nameColumn.getStorage().getItemBoxed(name_locs.get(0));
      String name;
      if (boxed == null) {
        throw Column.raiseNothingName();
      } else {
        name = boxed.toString();
        // We want to fail hard on invalid colum names stemming from invalid input values and make
        // the user fix the data before cross_tab, to avoid data corruption.
        Column.ensureNameIsValid(name);
      }

      for (int i = 0; i < aggregates.length; i++) {
        String effectiveName;
        if (aggregateNames[i].isEmpty()) {
          effectiveName = name;
        } else if (name.isEmpty()) {
          effectiveName = aggregateNames[i];
        } else {
          effectiveName = name + " " + aggregateNames[i];
        }

        // Check again to ensure that the appended aggregate name does not invalidate the name.
        // We do not check aggregateName itself before, because it _is_ allowed for it to be empty -
        // meaning just key names will be used and that is fine.
        Column.ensureNameIsValid(effectiveName);
        effectiveName = outputTableNameDeduplicator.makeUnique(effectiveName);

        output[offset + i] = new Column(effectiveName, storage[offset + i].seal());
      }

      offset += aggregates.length;
    }

    // Merge Problems
    AggregatedProblems[] problems = new AggregatedProblems[aggregates.length + 3];
    problems[0] = this.problems;
    problems[1] = AggregatedProblems.of(outputTableNameDeduplicator.getProblems());
    problems[2] = nameIndex.getProblems();
    for (int i = 0; i < aggregates.length; i++) {
      problems[i + 3] = aggregates[i].getProblems();
    }
    AggregatedProblems merged = AggregatedProblems.merge(problems);

    return new Table(output, merged);
  }

  public AggregatedProblems getProblems() {
    return problems;
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

  public Set<KeyType> keys() {
    return locs.keySet();
  }

  public boolean contains(KeyType key) {
    return this.locs.containsKey(key);
  }

  public List<Integer> get(KeyType key) {
    return this.locs.get(key);
  }
}
