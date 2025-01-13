package org.enso.table.operations;

import org.enso.table.data.column.storage.Storage;

public interface RunningStatistic<TypeStorage, TypeIterator> {

  TypeIterator getNewIterator();

  void calculateNextValue(int i, TypeIterator it);

  void finalise(TypeIterator it);

  Storage<TypeStorage> getResult();
}
