package org.enso.table.aggregations;

import java.util.List;
import org.enso.table.data.column.storage.type.IntegerType;

/** Aggregate Column counting the number of entries in a group. */
public class Count extends Aggregator {
  public Count(String name) {
    super(name, IntegerType.INT_64);
  }

  @Override
  public Object aggregate(int[] indexes) {
    return indexes.length;
  }

  @Override
  public Object aggregate(List<Integer> indexes) {
    return indexes.size();
  }
}
