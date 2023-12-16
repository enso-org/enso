package org.enso.table.data.column.operation.map.numeric;

import java.util.BitSet;
import org.enso.polyglot.common_utils.Core_Math_Utils;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

/** An operation rounding integers. */
public class LongRoundOp extends TernaryMapOperation<Long, AbstractLongStorage> {

  /** Minimum value for the `n` parameter to `roundLong`. */
  private static final long ROUND_MIN_LONG = -99999999999999L;

  /** Minimum value for the `n` parameter to `roundLong`. */
  private static final long ROUND_MAX_LONG = 99999999999999L;

  public LongRoundOp(String name) {
    super(name);
  }

  @Override
  public Storage<Long> runTernaryMap(
      AbstractLongStorage storage,
      Object decimalPlacesObject,
      Object useBankersObject,
      MapOperationProblemAggregator problemAggregator) {
    if (!(decimalPlacesObject instanceof Long decimalPlaces)) {
      throw new UnexpectedTypeException("a long.");
    }

    if (!(useBankersObject instanceof Boolean useBankers)) {
      throw new UnexpectedTypeException("a boolean.");
    }

    if (decimalPlaces >= 0) {
      // No change, return original storage.
      return storage;
    }

    Context context = Context.getCurrent();
    long[] out = new long[storage.size()];
    BitSet isMissing = new BitSet();

    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNa(i)) {
        long item = storage.getItem(i);
        boolean outOfRange = item < ROUND_MIN_LONG || item > ROUND_MAX_LONG;
        if (!outOfRange) {
          out[i] = Core_Math_Utils.roundLong(item, decimalPlaces, useBankers);
        } else {
          String msg =
              "Error: `round` can only accept values between "
                  + ROUND_MIN_LONG
                  + " and "
                  + ROUND_MAX_LONG
                  + " (inclusive), but was "
                  + item;
          problemAggregator.reportIllegalArgumentError(msg, i);
          isMissing.set(i);
        }

      } else {
        isMissing.set(i);
      }

      context.safepoint();
    }

    return new LongStorage(out, storage.size(), isMissing, IntegerType.INT_64);
  }
}
