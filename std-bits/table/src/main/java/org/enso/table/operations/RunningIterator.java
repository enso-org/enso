package org.enso.table.operations;

public interface RunningIterator {

  Double next(Double value);

  Double currentValue();
}
