package org.enso.table.operations;

import java.util.*;

import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.MultiValueKeyBase;
import org.enso.table.data.index.UnorderedMultiValueKey;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.util.ConstantList;

public class Distinct {
  /** Creates a row mask containing only the first row from sets of rows grouped by key columns. */
  public static BitSet buildDistinctRowsMask(
      int tableSize,
      Column[] keyColumns,
      TextFoldingStrategy textFoldingStrategy,
      AggregatedProblems problems) {
    var mask = new BitSet();
    if (keyColumns.length != 0) {
      HashSet<MultiValueKeyBase> visitedRows = new HashSet<>();
      int size = keyColumns[0].getSize();
      Storage<?>[] storage =
          Arrays.stream(keyColumns).map(Column::getStorage).toArray(Storage[]::new);
      List<TextFoldingStrategy> strategies = ConstantList.make(textFoldingStrategy, storage.length);
      for (int i = 0; i < size; i++) {
        UnorderedMultiValueKey key = new UnorderedMultiValueKey(storage, i, strategies);

        if (key.hasFloatValues()) {
          problems.add(new FloatingPointGrouping("Distinct", i));
        }

        if (!visitedRows.contains(key)) {
          mask.set(i);
          visitedRows.add(key);
        }
      }
    } else {
      // If there are no columns to distinct-by we just return the whole table.
      mask.set(0, tableSize);
    }

    return mask;
  }
}
