package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.numeric.NumericStorage;
import org.enso.table.error.UnexpectedTypeException;
import org.enso.table.util.BitSets;

import java.util.BitSet;

/**
 * An operation expecting a numeric argument and returning a boolean.
 */
public abstract class LongNumericOp extends MapOperation<Long, AbstractLongStorage> {
  private final boolean alwaysCastToDouble;

  public LongNumericOp(String name, boolean alwaysCastToDouble) {
    super(name);
    this.alwaysCastToDouble = alwaysCastToDouble;
  }

  public LongNumericOp(String name) {
    this(name, false);
  }

  public abstract double doDouble(long in, double arg, int ix, MapOperationProblemBuilder problemBuilder);

  public abstract Long doLong(long in, long arg, int ix, MapOperationProblemBuilder problemBuilder);

  @Override
  public NumericStorage<?> runMap(AbstractLongStorage storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      if (alwaysCastToDouble) {
        return DoubleStorage.makeEmpty(storage.size());
      } else {
        return LongStorage.makeEmpty(storage.size());
      }
    } else if (!alwaysCastToDouble && arg instanceof Long x) {
      BitSet newMissing = BitSets.makeDuplicate(storage.getIsMissing());
      long[] newVals = new long[storage.size()];
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          Long newVal = doLong(storage.getItem(i), x, i, problemBuilder);
          if (newVal == null) {
            newMissing.set(i);
          } else {
            newVals[i] = newVal;
          }
        }
      }
      return new LongStorage(newVals, newVals.length, newMissing);
    } else if (arg instanceof Double || arg instanceof Long) {
      double x = (arg instanceof Double) ? (Double) arg : (Long) arg;
      long[] newVals = new long[storage.size()];
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          newVals[i] = Double.doubleToRawLongBits(doDouble(storage.getItem(i), x, i, problemBuilder));
        }
      }
      return new DoubleStorage(newVals, newVals.length, storage.getIsMissing());
    }
    throw new UnexpectedTypeException("a Number");
  }

  @Override
  public NumericStorage<?> runZip(AbstractLongStorage storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    if (arg instanceof AbstractLongStorage v) {
      long[] out = new long[storage.size()];
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          if (alwaysCastToDouble) {
            out[i] = Double.doubleToRawLongBits(doDouble(storage.getItem(i), v.getItem(i), i, problemBuilder));
          } else {
            Long newVal = doLong(storage.getItem(i), v.getItem(i), i, problemBuilder);
            if (newVal == null) {
              newMissing.set(i);
            } else {
              out[i] = newVal;
            }
          }
        } else {
          newMissing.set(i);
        }
      }
      return alwaysCastToDouble ? new DoubleStorage(out, storage.size(), newMissing) : new LongStorage(out, storage.size(), newMissing);
    } else if (arg instanceof DoubleStorage v) {
      long[] out = new long[storage.size()];
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          out[i] = Double.doubleToRawLongBits(doDouble(storage.getItem(i), v.getItem(i), i, problemBuilder));
        } else {
          newMissing.set(i);
        }
      }
      return new DoubleStorage(out, storage.size(), newMissing);
    } else {
      throw new UnexpectedTypeException("a Number.");
    }
  }
}
