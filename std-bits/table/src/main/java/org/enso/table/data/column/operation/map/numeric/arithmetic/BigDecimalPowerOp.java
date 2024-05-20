package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.IntegerArrayAdapter;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

public class BigDecimalPowerOp extends BinaryMapOperation<BigDecimal, SpecializedStorage<BigDecimal>> {
  public BigDecimalPowerOp() {
    super("^");
  }

  @Override
  public Storage<?> runBinaryMap(
      SpecializedStorage<BigDecimal> storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(storage);
    Integer right = NumericConverter.tryConvertingToInteger(arg);
    if (right == null) {
        throw new IllegalArgumentException("Exponent must be an Integer.");
    }
    return runBigDecimalMap(left, right, problemAggregator);
  }

  @Override
  public Storage<?> runZip(
      SpecializedStorage<BigDecimal> storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(storage);
    IntegerArrayAdapter right = switch(arg) {
        case AbstractLongStorage lhs -> IntegerArrayAdapter.fromStorage(lhs);
        case BigIntegerStorage lhs -> IntegerArrayAdapter.fromStorage(lhs);
        default -> throw new IllegalStateException(
            "Unsupported storage: " + arg.getClass().getCanonicalName());
    };
    return runBigDecimalZip(left, right, problemAggregator);
  }

  private BigDecimalStorage runBigDecimalZip(
      BigDecimalArrayAdapter a,
      IntegerArrayAdapter b,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    BigDecimal[] out = new BigDecimal[n];
    for (int i = 0; i < m; i++) {
      BigDecimal x = a.getItem(i);
      Integer y = b.getItemAsInteger(i, problemAggregator);
      if (x != null && y != null) {
        BigDecimal r = x.pow(y);
        out[i] = r;
      }
      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }

  protected BigDecimalStorage runBigDecimalMap(
      BigDecimalArrayAdapter a, int b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    BigDecimal[] out = new BigDecimal[n];
    for (int i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      if (x == null) {
        out[i] = null;
      } else {
        BigDecimal r = x.pow(b);
        out[i] = r;
      }

      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }
}
