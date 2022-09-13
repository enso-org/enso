package org.enso.table.data.index;

import java.util.*;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.object.*;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.FloatingPointGrouping;

public class MultiValueIndex {
  private final int keyColumnsLength;
  private final Map<MultiValueKeyBase, List<Integer>> locs;
  private final AggregatedProblems problems;

  public MultiValueIndex(Column[] keyColumns, int tableSize, Comparator<Object> objectComparator) {
    this(keyColumns, tableSize, null, objectComparator);
  }

  public MultiValueIndex(
      Column[] keyColumns, int tableSize, int[] ordering, Comparator<Object> objectComparator) {
    this.keyColumnsLength = keyColumns.length;
    this.problems = new AggregatedProblems();

    boolean isOrdered = ordering != null;
    this.locs = isOrdered ? new TreeMap<>() : new HashMap<>();

    Storage[] storage = Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
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
            .map(c -> getBuilderForType(c.getType(), emptyScenario ? 1 : size))
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

  private static Builder getBuilderForType(int type, int size) {
    return switch (type) {
      case Storage.Type.OBJECT -> new ObjectBuilder(size);
      case Storage.Type.LONG -> NumericBuilder.createLongBuilder(size);
      case Storage.Type.DOUBLE -> NumericBuilder.createDoubleBuilder(size);
      case Storage.Type.STRING -> new StringBuilder(size);
      case Storage.Type.BOOL -> new BoolBuilder();
      case Storage.Type.DATE -> new DateBuilder(size);
      case Storage.Type.TIME_OF_DAY -> new TimeOfDayBuilder(size);
      case Storage.Type.DATE_TIME -> new DateTimeBuilder(size);
      default -> new InferredBuilder(size);
    };
  }
}
