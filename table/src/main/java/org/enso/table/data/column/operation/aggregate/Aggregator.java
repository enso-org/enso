package org.enso.table.data.column.operation.aggregate;

import org.enso.table.data.column.storage.Storage;

import java.util.List;

public abstract class Aggregator {
  public abstract void nextGroup(List<Integer> positions);

  public abstract Storage seal();
}
