package org.enso.table.aggregations;

import org.enso.table.data.column.storage.type.Integer;

import java.util.List;

/** Aggregate Column counting the number of entries in a group. */
public class Count extends Aggregator {
  public Count(String name) {
    super(name, Integer.INT_64);
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
