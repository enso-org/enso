package org.enso.table.data.column.operation.map;

import java.util.BitSet;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.graalvm.polyglot.Context;

/** An operation that takes a single double argumebnt and returns a long. */
public abstract class UnaryDoubleToLongOp extends UnaryMapOperation<Double, DoubleStorage> {

  public UnaryDoubleToLongOp(String name) {
    super(name);
  }

  protected abstract long doOperation(double value);

  @Override
  protected LongStorage run(DoubleStorage storage) {
    Context context = Context.getCurrent();
    BitSet newMissing = new BitSet();
    long[] newVals = new long[storage.size()];
    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        newVals[i] = doOperation(storage.getItem(i));
      } else {
        newMissing.set(i);
      }

      context.safepoint();
    }

    return new LongStorage(newVals, newVals.length, newMissing);
  }
}
