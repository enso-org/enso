package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.BitSet;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.BigIntegerArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.graalvm.polyglot.Context;

/**
 * A variant of NumericBinaryOpImplementation that has different null behaviour: if one of the
 * values is null, the other non-null value is returned.
 */
public abstract class NumericBinaryOpCoalescing<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpImplementation<T, I> {
  public NumericBinaryOpCoalescing(String name) {
    super(name);
  }

  @Override
  protected DoubleStorage runDoubleZip(
      DoubleArrayAdapter a, DoubleArrayAdapter b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < m; i++) {
      boolean aNothing = a.isNothing(i);
      boolean bNothing = b.isNothing(i);
      if (aNothing && bNothing) {
        isNothing.set(i);
      } else {
        double r;
        if (aNothing) {
          r = b.getItemAsDouble(i);
        } else if (bNothing) {
          r = a.getItemAsDouble(i);
        } else {
          r = doDouble(a.getItemAsDouble(i), b.getItemAsDouble(i), i, problemAggregator);
        }
        out[i] = Double.doubleToRawLongBits(r);
      }

      context.safepoint();
    }

    for (int i = m; i < n; ++i) {
      if (a.isNothing(i)) {
        isNothing.set(i);
      } else {
        out[i] = Double.doubleToRawLongBits(a.getItemAsDouble(i));
      }

      context.safepoint();
    }

    return new DoubleStorage(out, n, isNothing);
  }

  @Override
  protected DoubleStorage runDoubleMap(
      DoubleArrayAdapter a, Double b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a.intoStorage();
    }

    double bNonNull = b;
    Context context = Context.getCurrent();
    int n = a.size();
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      double r =
          a.isNothing(i)
              ? bNonNull
              : doDouble(a.getItemAsDouble(i), bNonNull, i, problemAggregator);
      out[i] = Double.doubleToRawLongBits(r);
      context.safepoint();
    }

    return new DoubleStorage(out, n, isNothing);
  }

  @Override
  protected LongStorage runLongZip(
      AbstractLongStorage a,
      AbstractLongStorage b,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < m; i++) {
      boolean aNothing = a.isNothing(i);
      boolean bNothing = b.isNothing(i);
      if (aNothing && bNothing) {
        isNothing.set(i);
      } else {
        if (aNothing) {
          out[i] = b.getItem(i);
        } else if (bNothing) {
          out[i] = a.getItem(i);
        } else {
          Long r = doLong(a.getItem(i), b.getItem(i), i, problemAggregator);
          if (r == null) {
            isNothing.set(i);
          } else {
            out[i] = r;
          }
        }
      }

      context.safepoint();
    }

    for (int i = m; i < n; ++i) {
      if (a.isNothing(i)) {
        isNothing.set(i);
      } else {
        out[i] = a.getItem(i);
      }

      context.safepoint();
    }

    return new LongStorage(out, n, isNothing, INTEGER_RESULT_TYPE);
  }

  protected Storage<Long> runLongMap(
      AbstractLongStorage a, Long b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a;
    }

    long bNonNull = b;
    Context context = Context.getCurrent();
    int n = a.size();
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (a.isNothing(i)) {
        out[i] = bNonNull;
      } else {
        Long r = doLong(a.getItem(i), bNonNull, i, problemAggregator);
        if (r == null) {
          isNothing.set(i);
        } else {
          out[i] = r;
        }
      }

      context.safepoint();
    }

    return new LongStorage(out, n, isNothing, INTEGER_RESULT_TYPE);
  }

  protected BigIntegerStorage runBigIntegerZip(
      BigIntegerArrayAdapter a,
      BigIntegerArrayAdapter b,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    BigInteger[] out = new BigInteger[n];
    for (int i = 0; i < m; i++) {
      BigInteger x = a.getItem(i);
      BigInteger y = b.getItem(i);
      if (x == null && y == null) {
        out[i] = null;
      } else {
        if (x == null) {
          out[i] = y;
        } else if (y == null) {
          out[i] = x;
        } else {
          BigInteger r = doBigInteger(x, y, i, problemAggregator);
          out[i] = r;
        }
      }
      context.safepoint();
    }

    return new BigIntegerStorage(out, n);
  }

  protected BigIntegerStorage runBigIntegerMap(
      BigIntegerArrayAdapter a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a.intoStorage();
    }

    Context context = Context.getCurrent();
    int n = a.size();
    BigInteger[] out = new BigInteger[n];
    for (int i = 0; i < n; i++) {
      BigInteger x = a.getItem(i);
      if (x == null) {
        out[i] = b;
      } else {
        BigInteger r = doBigInteger(x, b, i, problemAggregator);
        out[i] = r;
      }

      context.safepoint();
    }

    return new BigIntegerStorage(out, n);
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
      if (x == null && y == null) {
        out[i] = null;
      } else {
        if (x == null) {
          out[i] = y;
        } else if (y == null) {
          out[i] = x;
        } else {
          BigDecimal r = doBigDecimal(x, y, i, problemAggregator);
          out[i] = r;
        }
      }
      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }

  protected SpecializedStorage<BigDecimal> runBigDecimalMap(
      BigDecimalArrayAdapter a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a.intoStorage();
    }

    Context context = Context.getCurrent();
    int n = a.size();
    BigDecimal[] out = new BigDecimal[n];
    for (int i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      if (x == null) {
        out[i] = b;
      } else {
        BigDecimal r = doBigDecimal(x, b, i, problemAggregator);
        out[i] = r;
      }

      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }
}
