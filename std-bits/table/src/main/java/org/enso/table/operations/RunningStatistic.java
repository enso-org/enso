package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;

public interface RunningStatistic<T> {

  RunningIterator<T> getNewIterator();

  void calculateNextValue(int i, RunningIterator<T> it);

  Storage<T> getResult();
}
