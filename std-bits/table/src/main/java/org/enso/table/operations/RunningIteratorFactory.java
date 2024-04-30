package org.enso.table.operations;

public interface RunningIteratorFactory {

  RunningIterator getIterator();

  void calculateNextValue(int i, RunningIterator it);
}
