package org.enso.table.data.column.operation.map.numeric;

import org.enso.base.CompareException;

public abstract class DoubleComparison extends DoubleBooleanOp {
  public DoubleComparison(String name) {
    super(name);
  }

  @Override
  protected boolean doObject(double a, Object o) {
    throw new CompareException(a, o);
  }
}
