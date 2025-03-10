package org.enso.table.data.column.operation.map.numeric;

import org.enso.polyglot.common_utils.Core_Math_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
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
    var builder = Builder.getForLong(IntegerType.INT_64, storage.getSize(), problemAggregator);
    for (long i = 0; i < storage.getSize(); i++) {
      if (!storage.isNothing(i)) {
        long item = storage.getItemAsLong(i);
        boolean outOfRange = item < ROUND_MIN_LONG || item > ROUND_MAX_LONG;
        if (!outOfRange) {
          builder.appendLong(Core_Math_Utils.roundLong(item, decimalPlaces, useBankers));
        } else {
          String msg =
              "Error: `round` can only accept values between "
                  + ROUND_MIN_LONG
                  + " and "
                  + ROUND_MAX_LONG
                  + " (inclusive), but was "
                  + item;
          // ToDo: ProblemAggregator should accept a long instead of an int.
          problemAggregator.reportIllegalArgumentError(msg, (int) i);
          builder.appendNulls(1);
        }
      } else {
        builder.appendNulls(1);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
