package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a boolean. */
public abstract class DoubleBooleanOp extends MapOperation<Double, DoubleStorage> {
  public DoubleBooleanOp(String name) {
    super(name);
  }

  protected abstract boolean doDouble(double a, double b);

  protected boolean doObject(double a, Object o) {
    throw new UnexpectedTypeException("a Number");
  }

  private Double tryCast(Object arg) {
    if (arg instanceof Long) {
      return ((Long) arg).doubleValue();
    } else if (arg instanceof Double) {
      return (Double) arg;
    } else {
      return null;
    }
  }

  @Override
  public BoolStorage runMap(DoubleStorage storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    Double v = tryCast(arg);
    if (v != null) {
      double x = v;
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
  public BoolStorage runZip(DoubleStorage storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
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
          if (doDouble(storage.getItem(i), v.getItemDouble(i))) {
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
          Double x = tryCast(arg.getItemBoxed(i));
          if (x == null) {
            if (doObject(storage.getItem(i), arg.getItemBoxed(i))) {
              newVals.set(i);
            }
          } else {
            if (doDouble(storage.getItem(i), x)) {
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
