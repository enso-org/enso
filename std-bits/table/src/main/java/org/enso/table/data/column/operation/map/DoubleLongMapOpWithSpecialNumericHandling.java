package org.enso.table.data.column.operation.map;

import java.util.BitSet;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.graalvm.polyglot.Context;

public abstract class DoubleLongMapOpWithSpecialNumericHandling
    extends UnaryMapOperationWithProblemBuilder<Double, DoubleStorage> {
  public DoubleLongMapOpWithSpecialNumericHandling(String name) {
    super(name);
  }

  protected abstract long doOperation(double a);

  @Override
  public LongStorage run(
      DoubleStorage storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    long[] out = new long[storage.size()];
    BitSet isMissing = new BitSet();

    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        double item = storage.getItem(i);
        boolean special = Double.isNaN(item) || Double.isInfinite(item);
        if (!special) {
          out[i] = doOperation(item);
        } else {
          String msg = "Value is " + item;
          problemBuilder.reportArithmeticError(msg, i);
          isMissing.set(i);
        }
      } else {
        isMissing.set(i);
      }

      context.safepoint();
    }
    return new LongStorage(out, storage.size(), isMissing);
  }
}
