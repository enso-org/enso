package org.enso.table.data.column.operation.map.numeric;

import org.enso.base.CompareException;

public abstract class LongComparison extends LongBooleanOp {
  public LongComparison(String name) {
    super(name);
  }

  @Override
  protected boolean doObject(long a, Object b) {
    throw new CompareException(a, b);
  }
}
