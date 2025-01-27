package org.enso.table.data.column.operation.map.numeric;

import org.enso.polyglot.common_utils.Core_Math_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

/** An operation rounding floating-point numbers. */
public class DoubleRoundOp extends TernaryMapOperation<Double, DoubleStorage> {

  public DoubleRoundOp(String name) {
    super(name);
  }

  @Override
  public Storage<?> runTernaryMap(
      DoubleStorage storage,
      Object decimalPlacesObject,
      Object useBankersObject,
      MapOperationProblemAggregator problemAggregator) {
    if (!(decimalPlacesObject instanceof Long decimalPlaces)) {
      throw new UnexpectedTypeException("a long.");
    }

    if (!(useBankersObject instanceof Boolean useBankers)) {
      throw new UnexpectedTypeException("a boolean.");
    }

    Context context = Context.getCurrent();

    if (decimalPlaces <= 0) {
      // Return Long storage
      var longBuilder =
          Builder.getForLong(IntegerType.INT_64, storage.getSize(), problemAggregator);

      for (long i = 0; i < storage.getSize(); i++) {
        if (!storage.isNothing(i)) {
          double item = storage.getItemAsDouble(i);
          boolean special = Double.isNaN(item) || Double.isInfinite(item);
          if (!special) {
            longBuilder.appendLong(
                (long) Core_Math_Utils.roundDouble(item, decimalPlaces, useBankers));
          } else {
            String msg = "Value is " + item;
            // ToDo: ProblemAggregator should accept a long instead of an int.
            problemAggregator.reportArithmeticError(msg, (int) i);
            longBuilder.appendNulls(1);
          }
        } else {
          longBuilder.appendNulls(1);
        }

        context.safepoint();
      }
      return longBuilder.seal();
    } else {
      // Return double storage.
      var doubleBuilder =
          Builder.getForDouble(FloatType.FLOAT_64, storage.getSize(), problemAggregator);

      for (long i = 0; i < storage.getSize(); i++) {
        if (!storage.isNothing(i)) {
          double item = storage.getItemAsDouble(i);
          boolean special = Double.isNaN(item) || Double.isInfinite(item);
          if (!special) {
            doubleBuilder.appendDouble(
                Core_Math_Utils.roundDouble(item, decimalPlaces, useBankers));
          } else {
            String msg = "Value is " + item;
            // ToDo: ProblemAggregator should accept a long instead of an int.
            problemAggregator.reportArithmeticError(msg, (int) i);
            doubleBuilder.appendNulls(1);
          }
        } else {
          doubleBuilder.appendNulls(1);
        }

        context.safepoint();
      }
      return doubleBuilder.seal();
    }
  }
}
