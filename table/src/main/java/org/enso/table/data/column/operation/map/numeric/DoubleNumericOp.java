package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;

public abstract class DoubleNumericOp extends MapOperation<DoubleStorage> {

  public DoubleNumericOp(String name) {
    super(name);
  }

  protected abstract double runDouble(double a, double b);

  @Override
  public Storage run(DoubleStorage storage, Object arg) {
    double x;
    if (arg instanceof Double) {
      x = (Double) arg;
    } else if (arg instanceof Long) {
      x = (Long) arg;
    } else {
      throw new UnexpectedTypeException("a Number.");
    }
    long[] out = new long[storage.size()];
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        out[i] = Double.doubleToRawLongBits(runDouble(storage.getItem(i), x));
      }
    }
    return new DoubleStorage(out, storage.size(), storage.getIsMissing());
  }
}
