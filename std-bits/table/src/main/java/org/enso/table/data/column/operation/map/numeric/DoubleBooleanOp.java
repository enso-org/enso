package org.enso.table.data.column.operation.map.numeric;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a boolean. */
public abstract class DoubleBooleanOp extends BinaryMapOperation<Double, DoubleStorage> {
  public DoubleBooleanOp(String name) {
    super(name);
  }

  protected abstract boolean doDouble(double a, double b);

  protected boolean doObject(double a, Object o) {
    throw new UnexpectedTypeException("a Number");
  }

  @Override
  public BoolStorage runBinaryMap(DoubleStorage storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    Double v = NumericConverter.tryConvertingToDouble(arg);
    if (v != null) {
      double x = v;
      BitSet newVals = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i)) {
          if (doDouble(storage.getItem(i), x)) {
            newVals.set(i);
          }
        }

        context.safepoint();
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

        context.safepoint();
      }
      return new BoolStorage(newVals, storage.getIsMissing(), storage.size(), false);
    }
  }

  @Override
  public BoolStorage runZip(DoubleStorage storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
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

        context.safepoint();
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    } else if (arg instanceof AbstractLongStorage v) {
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          double left = storage.getItem(i);
          long right = v.getItem(i);
          // We convert from long to double here. This may lose precision, but we do not report
          // LossOfIntegerPrecision, because it is expected that numeric operations involving floating point columns
          // are inherently imprecise.
          double rightConverted = (double) right;
          if (doDouble(left, rightConverted)) {
            newVals.set(i);
          }
        } else {
          newMissing.set(i);
        }

        context.safepoint();
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    } else {
      BitSet newVals = new BitSet();
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < arg.size() && !arg.isNa(i)) {
          Double x = NumericConverter.tryConvertingToDouble(arg.getItemBoxed(i));
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

        context.safepoint();
      }
      return new BoolStorage(newVals, newMissing, storage.size(), false);
    }
  }

  @Override
  public boolean reliesOnSpecializedStorage() {
    return false;
  }
}
