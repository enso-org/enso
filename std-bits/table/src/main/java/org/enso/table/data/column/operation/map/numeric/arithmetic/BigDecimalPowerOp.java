package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;

import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.graalvm.polyglot.Context;

public class BigDecimalPowerOp extends BinaryMapOperation<BigDecimal, BigDecimalStorage> {
  public BigDecimalPowerOp() {
    super("/");
  }

  @Override
  public Storage<?> runBinaryMap(
      BigDecimalStorage storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    throw new UnsupportedOperationException("");
  }

  @Override
  public abstract Storage<?> runZip(
      BigDecimalStorage storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(storage);)
    BigDecimalArrayAdapter right = switch(arg) {
        case AbstractLongStorage lhs -> BigDecimalArrayAdapter.fromStorage(lhs);
        case BigIntegerStorage lhs -> BigDecimalArrayAdapter.fromStorage(lhs);
        default -> throw new IllegalStateException(
            "Unsupported storage: " + arg.getClass().getCanonicalName());
    }
    return runBigDecimalZip((left, right, problemAggregator)
  }

  protected BigDecimalStorage runBigDecimalZip(
      BigDecimalArrayAdapter a,
      BigDecimalArrayAdapter b,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    BigDecimal[] out = new BigDecimal[n];
    for (int i = 0; i < m; i++) {
      BigDecimal x = a.getItem(i);
      BigDecimal y = b.getItem(i);
      if (x != null && y != null) {
        BigDecimal r = doBigDecimal(x, y, i, problemAggregator);
        out[i] = r;
      }
      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }
}
