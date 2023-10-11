package org.enso.table.data.column.operation.map.numeric;

import java.util.BitSet;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.graalvm.polyglot.Context;

/** An operation that takes a single double argument and returns a long. */
public abstract class UnaryLongToLongOp extends UnaryMapOperation<Long, AbstractLongStorage> {

  public UnaryLongToLongOp(String name) {
    super(name);
  }

  protected abstract long doOperation(long value);

  @Override
  protected LongStorage runUnaryMap(
      AbstractLongStorage storage, MapOperationProblemAggregator problemBuilder) {
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

    // TODO is inheriting type ok? it may not be enough!
    return new LongStorage(newVals, newVals.length, newMissing, storage.getType());
  }
}
