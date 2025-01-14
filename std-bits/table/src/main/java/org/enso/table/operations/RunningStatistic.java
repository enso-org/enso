package org.enso.table.operations;

public interface RunningStatistic<TypeIterator> {

  TypeIterator getNewIterator();

  void calculateNextValue(int i, TypeIterator it);

}
