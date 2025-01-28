package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.BigIntegerArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
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
  protected Storage<Double> runDoubleZip(
      DoubleArrayAdapter a, DoubleArrayAdapter b, MapOperationProblemAggregator problemAggregator) {
    long n = a.size();
    long m = Math.min(n, b.size());
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    Context context = Context.getCurrent();

    for (long i = 0; i < n; i++) {
      boolean aNothing = a.isNothing(i);
      boolean bNothing = i >= m || b.isNothing(i);
      if (aNothing && bNothing) {
        builder.appendNulls(1);
      } else {
        double r;
        if (aNothing) {
          r = b.getItemAsDouble(i);
        } else if (bNothing) {
          r = a.getItemAsDouble(i);
        } else {
          r = doDouble(a.getItemAsDouble(i), b.getItemAsDouble(i), i, problemAggregator);
        }
        builder.appendDouble(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  @Override
  protected Storage<Double> runDoubleMap(
      DoubleArrayAdapter a, Double b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a.intoStorage();
    }

    double bNonNull = b;
    Context context = Context.getCurrent();
    long n = a.size();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    for (long i = 0; i < n; i++) {
      builder.appendDouble(
          a.isNothing(i)
              ? bNonNull
              : doDouble(a.getItemAsDouble(i), bNonNull, i, problemAggregator));
      context.safepoint();
    }

    return builder.seal();
  }

  @Override
  protected Storage<Long> runLongZip(
      AbstractLongStorage a,
      AbstractLongStorage b,
      MapOperationProblemAggregator problemAggregator) {
    long n = a.getSize();
    long m = Math.min(n, b.getSize());
    var builder = Builder.getForLong(IntegerType.INT_64, n, problemAggregator);
    Context context = Context.getCurrent();

    for (long i = 0; i < n; i++) {
      boolean aNothing = a.isNothing(i);
      boolean bNothing = i >= m || b.isNothing(i);
      if (aNothing && bNothing) {
        builder.appendNulls(1);
      } else {
        if (aNothing) {
          builder.appendLong(b.getItemAsLong(i));
        } else if (bNothing) {
          builder.appendLong(a.getItemAsLong(i));
        } else {
          Long r = doLong(a.getItemAsLong(i), b.getItemAsLong(i), i, problemAggregator);
          if (r == null) {
            builder.appendNulls(1);
          } else {
            builder.appendLong(r);
          }
        }
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Long> runLongMap(
      AbstractLongStorage a, Long b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a;
    }

    long bNonNull = b;
    Context context = Context.getCurrent();
    long n = a.getSize();
    var builder = Builder.getForLong(IntegerType.INT_64, n, problemAggregator);
    for (long i = 0; i < n; i++) {
      if (a.isNothing(i)) {
        builder.appendLong(bNonNull);
      } else {
        Long r = doLong(a.getItemAsLong(i), bNonNull, i, problemAggregator);
        if (r == null) {
          builder.appendNulls(1);
        } else {
          builder.appendLong(r);
        }
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<BigInteger> runBigIntegerZip(
      BigIntegerArrayAdapter a,
      BigIntegerArrayAdapter b,
      MapOperationProblemAggregator problemAggregator) {
    long n = a.size();
    long m = Math.min(n, b.size());
    var builder = Builder.getForBigInteger(n, problemAggregator);
    Context context = Context.getCurrent();

    for (long i = 0; i < n; i++) {
      BigInteger x = a.getItem(i);
      BigInteger y = i >= m ? null : b.getItem(i);
      if (x == null && y == null) {
        builder.appendNulls(1);
      } else {
        if (x == null) {
          builder.append(y);
        } else if (y == null) {
          builder.append(x);
        } else {
          BigInteger r = doBigInteger(x, y, i, problemAggregator);
          builder.append(r);
        }
      }
      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<BigInteger> runBigIntegerMap(
      BigIntegerArrayAdapter a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a.intoStorage();
    }

    Context context = Context.getCurrent();
    long n = a.size();
    var builder = Builder.getForBigInteger(n, problemAggregator);
    for (long i = 0; i < n; i++) {
      BigInteger x = a.getItem(i);
      if (x == null) {
        builder.append(b);
      } else {
        builder.append(doBigInteger(x, b, i, problemAggregator));
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<BigDecimal> runBigDecimalZip(
      BigDecimalArrayAdapter a,
      BigDecimalArrayAdapter b,
      MapOperationProblemAggregator problemAggregator) {
    long n = a.size();
    long m = Math.min(a.size(), b.size());
    var builder = Builder.getForBigDecimal(n);
    Context context = Context.getCurrent();

    for (long i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      BigDecimal y = i >= m ? null : b.getItem(i);
      if (x == null && y == null) {
        builder.appendNulls(1);
      } else {
        if (x == null) {
          builder.append(y);
        } else if (y == null) {
          builder.append(x);
        } else {
          builder.append(doBigDecimal(x, y, i, problemAggregator));
        }
      }
      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<BigDecimal> runBigDecimalMap(
      BigDecimalArrayAdapter a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return a.intoStorage();
    }

    Context context = Context.getCurrent();
    long n = a.size();
    var builder = Builder.getForBigDecimal(n);
    for (long i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      if (x == null) {
        builder.append(b);
      } else {
        builder.append(doBigDecimal(x, b, i, problemAggregator));
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
