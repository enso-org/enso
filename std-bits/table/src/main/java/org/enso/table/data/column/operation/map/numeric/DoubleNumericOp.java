package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a number. */
public abstract class DoubleNumericOp extends MapOperation<Double, DoubleStorage> {

  public DoubleNumericOp(String name) {
    super(name);
  }

  protected abstract double doDouble(double a, double b, int ix, MapOperationProblemBuilder problemBuilder);

  @Override
  public Storage<Double> runMap(DoubleStorage storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      return DoubleStorage.makeEmpty(storage.size());
    }

    double x;
    if (arg instanceof Double) {
      x = (Double) arg;
    } else if (arg instanceof Long) {
      x = (Long) arg;
    } else {
      throw new UnexpectedTypeException("a Number.");
    }

    Context context = Context.getCurrent();
    long[] out = new long[storage.size()];
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        out[i] = Double.doubleToRawLongBits(doDouble(storage.getItem(i), x, i, problemBuilder));
      }

      context.safepoint();
    }
    return new DoubleStorage(out, storage.size(), storage.getIsMissing());
  }

  @Override
  public Storage<Double> runZip(DoubleStorage storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    if (arg instanceof LongStorage v) {
      long[] out = new long[storage.size()];
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          out[i] = Double.doubleToRawLongBits(doDouble(storage.getItem(i), v.getItem(i), i, problemBuilder));
        } else {
          newMissing.set(i);
        }

        context.safepoint();
      }
      return new DoubleStorage(out, storage.size(), newMissing);
    } else if (arg instanceof DoubleStorage v) {
      long[] out = new long[storage.size()];
      BitSet newMissing = new BitSet();
      for (int i = 0; i < storage.size(); i++) {
        if (!storage.isNa(i) && i < v.size() && !v.isNa(i)) {
          out[i] = Double.doubleToRawLongBits(doDouble(storage.getItem(i), v.getItem(i), i, problemBuilder));
        } else {
          newMissing.set(i);
        }

        context.safepoint();
      }
      return new DoubleStorage(out, storage.size(), newMissing);
    } else {
      throw new UnexpectedTypeException("a Number.");
    }
  }
}
