package org.enso.table.data.table.aggregate;

import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.index.Index;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;

import java.util.Arrays;
import java.util.List;

public class AggregateTable {
  private final Table table;
  private final Index uniqueIndex;

  public AggregateTable(Table table) {
    this.table = table;
    this.uniqueIndex = table.getIndex().unique();
  }

  public Column count() {
    long[] counts = new long[uniqueIndex.size()];
    for (int i = 0; i < uniqueIndex.size(); i++) {
      List<Integer> items = table.getIndex().loc(uniqueIndex.iloc(i));
      counts[i] = items == null ? 0 : items.size();
    }
    LongStorage storage = new LongStorage(counts);
    return new Column("count", uniqueIndex, storage);
  }

  public AggregateColumn getColumnByName(String n) {
    Column c = table.getColumnByName(n);
    if (c == null) {
      return null;
    } else {
      return new AggregateColumn(uniqueIndex, c);
    }
  }

  public AggregateColumn[] getColumns() {
    return Arrays.stream(table.getColumns())
        .map(c -> new AggregateColumn(uniqueIndex, c))
        .toArray(AggregateColumn[]::new);
  }
}
