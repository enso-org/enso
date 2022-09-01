package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.problems.WithProblems;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Distinct {
  public static BitSet buildDistinctRowsMask(int tableSize, Column[] keyColumns, Comparator<Object> objectComparator, AggregatedProblems problems) {
    var mask = new BitSet();
    if (keyColumns.length != 0) {
      Map<MultiValueKey, Boolean> visitedRows = new HashMap<>();
      int size = keyColumns[0].getSize();
      Storage[] storage = Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
      for (int i = 0; i < size; i++) {
        MultiValueKey key = new MultiValueKey(storage, i, null, objectComparator);

        if (key.hasFloatValues()) {
          problems.add(new FloatingPointGrouping("Distinct", i));
        }

        if (!visitedRows.containsKey(key)) {
          mask.set(i);
          visitedRows.put(key, true);
        }
      }
    } else {
      // If there are no columns to distinct-by we just return the whole table.
      mask.set(0, tableSize);
    }

    return mask;
  }
}
