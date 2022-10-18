package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a boolean. */
public abstract class LongBooleanOp extends MapOperation<Long, LongStorage> {
  public LongBooleanOp(String name) {
    super(name);
  }

  protected abstract boolean doLong(long a, long b);

  protected abstract boolean doDouble(long a, double b);

  protected boolean doObject(long a, Object b) {
    throw new UnexpectedTypeException("a Number");
  }

  @Override
  public BoolStorage runMap(LongStorage storage, Object arg) {
    if (arg instanceof Long) {
      long x = (Long) arg;
      BitSet newVals = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          if (doLong(storage.getItem(i), x)) {
            newVals.set(i);
          }
        }
      }
      return new BoolStorage(newVals, storage.getIsMissing(), storage.size(), false);
    } else if (arg instanceof Double) {
      double x = (Double) arg;
      BitSet newVals = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          if (doDouble(storage.getItem(i), x)) {
            newVals.set(i);
          }
        }
      }
      return new BoolStorage(newVals, storage.getIsMissing(), storage.size(), false);
    } else {
      BitSet newVals = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          if (doObject(storage.getItem(i), arg)) {
            newVals.set(i);
          }
        }
      }
      return new BoolStorage(newVals, storage.getIsMissing(), storage.size(), false);
    }
  }

  @Override
  public BoolStorage runZip(LongStorage storage, Storage<?> arg) {
    if (arg instanceof DoubleStorage v) {
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          if (doDouble(storage.getItem(i), v.getItem(i))) {
            newVals.set(i);
          }
        } else {
          newMissing.set(i);
        }
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    } else if (arg instanceof LongStorage v) {
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          if (doLong(storage.getItem(i), v.getItem(i))) {
            newVals.set(i);
          }
        } else {
          newMissing.set(i);
        }
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    } else {
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < arg.size() && !arg.isNa(i)) {
          Object v = arg.getItemBoxed(i);
          if (v instanceof Long) {
            if (doLong(storage.getItem(i), (Long) v)) {
              newVals.set(i);
            }
          } else if (v instanceof Double) {
            if (doDouble(storage.getItem(i), (Double) v)) {
              newVals.set(i);
            }
          } else {
            if (doObject(storage.getItem(i), v)) {
              newVals.set(i);
            }
          }
        } else {
          newMissing.set(i);
        }
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    }
  }
}
