package org.enso.table.data.index;

import java.util.*;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.error.TooManyColumnsException;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.util.ConstantList;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Context;

public class MultiValueIndex<KeyType extends MultiValueKeyBase> {
  private static final int MAXIMUM_CROSS_TAB_COLUMN_COUNT = 10000;

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

      Context context = Context.getCurrent();
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

        context.safepoint();
      }
    } else {
      this.locs.put(
          keyFactory.apply(0), IntStream.range(0, tableSize).boxed().collect(Collectors.toList()));
    }
  }

  public Table makeTable(Aggregator[] columns) {
    Context context = Context.getCurrent();
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
        context.safepoint();
      }
    } else {
      for (List<Integer> group_locs : this.locs.values()) {
        for (int i = 0; i < length; i++) {
          Object value = columns[i].aggregate(group_locs);
          storage[i].appendNoGrow(value);
          context.safepoint();
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

  public static Table makeCrossTabTable(
      Column[] groupingColumns,
      Column nameColumn,
      Aggregator[] aggregates,
      String[] aggregateNames) {
    Context context = Context.getCurrent();
    NameDeduplicator outputTableNameDeduplicator = new NameDeduplicator();

    // Create combined index
    Column combinedColumns[] = Stream.concat(Arrays.stream(groupingColumns), Arrays.stream(new Column[] {nameColumn}))
            .toArray(Column[]::new);
    var combinedIndex =
            MultiValueIndex.makeUnorderedIndex(
                    combinedColumns,
                    nameColumn.getSize(),
                    TextFoldingStrategy.unicodeNormalizedFold);
    List<UnorderedMultiValueKey> combinedKeys = new ArrayList<>(combinedIndex.locs.keySet());
    // Generate subkeys
    List<UnorderedMultiValueKey> groupingSubkeys = new ArrayList<>(combinedKeys.size());
    List<UnorderedMultiValueKey> nameSubKeys = new ArrayList<>(combinedKeys.size());
    int[] groupingColumnIndices = IntStream.range(0, groupingColumns.length).toArray();
    int[] nameColumnIndices = new int[] { groupingColumns.length };
    for (var key : combinedKeys) {
      groupingSubkeys.add(key.subKey(groupingColumnIndices));
      nameSubKeys.add(key.subKey(nameColumnIndices));
    }

    // Fill numberings
    ObjectNumberer<UnorderedMultiValueKey> nameNumberer = new ObjectNumberer<>();
    for (UnorderedMultiValueKey key : nameSubKeys) {
      nameNumberer.put(key);
    }
    ObjectNumberer<UnorderedMultiValueKey> groupingNumberer = new ObjectNumberer<>();
    for (UnorderedMultiValueKey key : groupingSubkeys) {
      groupingNumberer.put(key);
    }

    final int size = groupingNumberer.size();

    final int columnCount = groupingColumns.length + nameNumberer.size() * aggregates.length;
    if (columnCount > MAXIMUM_CROSS_TAB_COLUMN_COUNT) {
      throw new TooManyColumnsException(
          "The cross_tab contained too many columns. Maximum allowed is "
              + MAXIMUM_CROSS_TAB_COLUMN_COUNT
              + " but was "
              + columnCount
              + ".",
          columnCount,
          MAXIMUM_CROSS_TAB_COLUMN_COUNT);
    }

    // Create the storage
    Builder[] storage = new Builder[columnCount];
    for (int i = 0; i < groupingColumns.length; i++) {
      storage[i] = Builder.getForType(groupingColumns[i].getStorage().getType(), size);
      context.safepoint();
    }

    //ArrayList<Integer>[] a = new ArrayList<Integer>[2];
    int numNames = nameNumberer.size();
    int numGroupings = groupingNumberer.size();
    //List<Integer>[][] subPartitions = (List<Integer>[][]) new ArrayList<Integer>[numGroupings][numNames];
    UnorderedMultiValueKey[][] arrangement = new UnorderedMultiValueKey[numGroupings][numNames];

    for (int i = 0; i < nameNumberer.size(); i++) {
      int offset = groupingColumns.length + i * aggregates.length;
      for (int j = 0; j < aggregates.length; j++) {
        storage[offset + j] = Builder.getForType(aggregates[j].getType(), size);
        context.safepoint();
      }
    }

    for (int i = 0; i < combinedIndex.size(); ++i) {
      UnorderedMultiValueKey combinedKey = combinedKeys.get(i);
      UnorderedMultiValueKey groupingSubKey = groupingSubkeys.get(i);
      UnorderedMultiValueKey nameSubKey = nameSubKeys.get(i);
      int groupingCoordinate = groupingNumberer.getNumber(groupingSubKey);
      int nameCoordinate = nameNumberer.getNumber(nameSubKey);
      // The pair (groupingCoordinate, nameCoordinate) must be unique so this
      // check is not really necessary.
      if (arrangement[groupingCoordinate][nameCoordinate] != null) {
        throw new IllegalStateException("Internal error: makeCrossTabTable coordinate conflict");
      }
      arrangement[groupingCoordinate][nameCoordinate] = combinedKey;
    }

    for (UnorderedMultiValueKey groupingSubKey : groupingNumberer.getObjects()) {
      int groupingCoordinate = groupingNumberer.getNumber(groupingSubKey);

      // Find a cell that has a key; there has to be one.
      UnorderedMultiValueKey keyForGroup = null;
      for (int i=0; i<numNames; ++i) {
        if (arrangement[groupingCoordinate][i] != null) {
            keyForGroup = arrangement[groupingCoordinate][i];
            break;
        }
      }
      if (keyForGroup == null) {
        throw new IllegalStateException("Internal error: empty group partition");
      }

      // Fill the grouping columns
      int aGroupRow = combinedIndex.locs.get(keyForGroup).get(0);
      IntStream.range(0, groupingColumns.length)
          .forEach(
                i ->
                    storage[i].appendNoGrow(
                        groupingColumns[i].getStorage().getItemBoxed(aGroupRow)));

      int offset = groupingColumns.length;

      for (UnorderedMultiValueKey nameSubKey : nameNumberer.getObjects()) {
        int nameCoordinate = nameNumberer.getNumber(nameSubKey);
        if (arrangement[groupingCoordinate][nameCoordinate] == null) {
          //System.out.println("AAAA0 null");
        }
        List<Integer> rowIds = combinedIndex.locs.get(arrangement[groupingCoordinate][nameCoordinate]);
        if (rowIds == null) {
          //System.out.println("AAAA1 null");
          rowIds = new ArrayList<Integer>();
        }

        for (int i = 0; i < aggregates.length; i++) {
          storage[offset + i].appendNoGrow(aggregates[i].aggregate(rowIds));
        }

        offset += aggregates.length;
        context.safepoint();
      }
    }

    // Create Columns
    Column[] output = new Column[columnCount];
    for (int i = 0; i < groupingColumns.length; i++) {
      outputTableNameDeduplicator.markUsed(groupingColumns[i].getName());
      output[i] = new Column(groupingColumns[i].getName(), storage[i].seal());
      context.safepoint();
    }

    int offset = groupingColumns.length;
    for (UnorderedMultiValueKey nameSubKey : nameNumberer.getObjects()) {
      Object boxed = nameSubKey.get(0);
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
        context.safepoint();
      }

      offset += aggregates.length;
    }

    // Merge Problems
    AggregatedProblems[] problems = new AggregatedProblems[aggregates.length + 2];
    problems[0] = combinedIndex.problems;
    problems[1] = AggregatedProblems.of(outputTableNameDeduplicator.getProblems());
    for (int i = 0; i < aggregates.length; i++) {
      problems[i + 2] = aggregates[i].getProblems();
      context.safepoint();
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
    Context context = Context.getCurrent();
    for (List<Integer> rowIndexes : this.locs.values()) {
      for (Integer rowIndex : rowIndexes) {
        output[idx++] = rowIndex;
        context.safepoint();
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

  public int size() {
    return this.locs.size();
  }
}
