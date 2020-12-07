package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.error.UnexpectedTypeException;

import java.util.BitSet;

public abstract class LongBooleanOp extends MapOperation<LongStorage> {
  public LongBooleanOp(String name) {
    super(name);
  }

  protected abstract boolean doLong(long a, long b);

  protected abstract boolean doDouble(long a, double b);

  protected BoolStorage doObject(LongStorage storage, Object o) {
    throw new UnexpectedTypeException("a Number");
  }

  @Override
  public BoolStorage run(LongStorage storage, Object arg) {
    BitSet newVals = new BitSet();
    if (arg instanceof Long) {
      long x = (Long) arg;
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          if (doLong(storage.getItem(i), x)) {
            newVals.set(i);
          }
        }
      }
    } else if (arg instanceof Double) {
      double x = (Double) arg;
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          if (doDouble(storage.getItem(i), x)) {
            newVals.set(i);
          }
        }
      }
    } else {
      return doObject(storage, arg);
    }
    return new BoolStorage(newVals, storage.getIsMissing(), storage.size(), false);
  }
}
