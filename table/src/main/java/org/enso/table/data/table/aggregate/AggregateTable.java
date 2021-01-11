package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.index.Index;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;

import java.util.Arrays;
import java.util.List;

/** Represents a table grouped by a given index. */
public class AggregateTable {
  private final Table table;
  private final Index uniqueIndex;

  /** @param table the underlying table */
  public AggregateTable(Table table) {
    this.table = table;
    this.uniqueIndex = table.getIndex().unique();
  }

  /** @return a column containing group sizes in this aggregate. */
  public Column count() {
    long[] counts = new long[uniqueIndex.size()];
    for (int i = 0; i < uniqueIndex.size(); i++) {
      List<Integer> items = table.getIndex().loc(uniqueIndex.iloc(i));
      counts[i] = items == null ? 0 : items.size();
    }
    LongStorage storage = new LongStorage(counts);
    return new Column("count", uniqueIndex, storage);
  }

  /**
   * Returns a column with the given name.
   *
   * @param n the column name
   * @return column with the given name or null if does not exist
   */
  public AggregateColumn getColumnByName(String n) {
    Column c = table.getColumnByName(n);
    if (c == null) {
      return null;
    } else {
      return new AggregateColumn(uniqueIndex, c);
    }
  }

  /** @return Aggregate columns contained in this table. */
  public AggregateColumn[] getColumns() {
    return Arrays.stream(table.getColumns())
        .map(c -> new AggregateColumn(uniqueIndex, c))
        .toArray(AggregateColumn[]::new);
  }
}
