package org.enso.table.operations;

public interface RunningIterator<T> {

  T next(T value);

  T currentValue();
}
