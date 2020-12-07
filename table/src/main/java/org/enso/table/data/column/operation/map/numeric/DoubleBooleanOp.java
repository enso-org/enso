package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.error.UnexpectedTypeException;

import java.util.BitSet;

public abstract class DoubleBooleanOp extends MapOperation<DoubleStorage> {
  public DoubleBooleanOp(String name) {
    super(name);
  }

  protected abstract boolean doDouble(double a, double b);

  protected BoolStorage doObject(DoubleStorage storage, Object o) {
    throw new UnexpectedTypeException("a Number");
  }

  @Override
  public BoolStorage run(DoubleStorage storage, Object arg) {
    BitSet newVals = new BitSet();
    double x;
    if (arg instanceof Double) {
      x = (Double) arg;
    } else if (arg instanceof Long) {
      x = (Long) arg;
    } else {
      return doObject(storage, arg);
    }
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        if (doDouble(storage.getItem(i), x)) {
          newVals.set(i);
        }
      }
    }
    return new BoolStorage(newVals, storage.getIsMissing(), storage.size(), false);
  }
}
