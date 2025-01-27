package org.enso.table.data.column.operation.map.numeric.comparisons;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.CompareException;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.BigIntegerArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.graalvm.polyglot.Context;

public abstract class NumericComparison<T extends Number, I extends Storage<? super T>>
    extends BinaryMapOperation<T, I> {

  protected abstract boolean doDouble(double a, double b);

  protected abstract boolean doLong(long a, long b);

  protected abstract boolean doBigInteger(BigInteger a, BigInteger b);

  protected abstract boolean doBigDecimal(BigDecimal a, BigDecimal b);

  protected boolean onOtherType(Object a, Object b) {
    throw new CompareException(a, b);
  }

  public NumericComparison(String name) {
    super(name);
  }

  @Override
  public Storage<Boolean> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.getSize());
    } else if (arg instanceof BigInteger bigInteger) {
      return switch (storage) {
        case AbstractLongStorage s -> runBigIntegerMap(
            BigIntegerArrayAdapter.fromStorage(s), bigInteger, problemAggregator);
        case BigIntegerStorage s -> runBigIntegerMap(
            BigIntegerArrayAdapter.fromStorage(s), bigInteger, problemAggregator);
        case BigDecimalStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromBigDecimalStorage(s),
            new BigDecimal(bigInteger),
            problemAggregator);
        case DoubleStorage s -> runDoubleMap(s, bigInteger.doubleValue(), problemAggregator);
        default -> throw new IllegalStateException(
            "Unsupported lhs storage: " + storage.getClass().getCanonicalName());
      };
    } else if (arg instanceof BigDecimal bigDecimal) {
      return switch (storage) {
        case AbstractLongStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromStorage(s), bigDecimal, problemAggregator);
        case BigIntegerStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromBigIntegerStorage(s), bigDecimal, problemAggregator);
        case BigDecimalStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromBigDecimalStorage(s), bigDecimal, problemAggregator);
        case DoubleStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromStorage(s), bigDecimal, problemAggregator);
        default -> throw new IllegalStateException(
            "Unsupported lhs storage: " + storage.getClass().getCanonicalName());
      };
    } else if (NumericConverter.isCoercibleToLong(arg)) {
      long rhs = NumericConverter.coerceToLong(arg);
      return switch (storage) {
        case AbstractLongStorage s -> runLongMap(s, rhs, problemAggregator);
        case BigIntegerStorage s -> runBigIntegerMap(
            BigIntegerArrayAdapter.fromStorage(s), BigInteger.valueOf(rhs), problemAggregator);
        case BigDecimalStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromBigDecimalStorage(s),
            BigDecimal.valueOf(rhs),
            problemAggregator);
        case DoubleStorage s -> runDoubleMap(s, (double) rhs, problemAggregator);
        default -> throw new IllegalStateException(
            "Unsupported lhs storage: " + storage.getClass().getCanonicalName());
      };
    } else if (NumericConverter.isCoercibleToDouble(arg)) {
      double rhs = NumericConverter.coerceToDouble(arg);
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(
            BigDecimalArrayAdapter.fromBigDecimalStorage(s),
            BigDecimal.valueOf(rhs),
            problemAggregator);
        default -> {
          DoubleArrayAdapter lhs = DoubleArrayAdapter.fromAnyStorage(storage);
          yield runDoubleMap(lhs, rhs, problemAggregator);
        }
      };
    } else {
      long n = storage.getSize();
      var builder = Builder.getForBoolean(n);
      Context context = Context.getCurrent();
      for (int i = 0; i < n; ++i) {
        Object item = storage.getItemBoxed(i);
        if (item == null) {
          builder.appendNulls(1);
        } else {
          boolean r = onOtherType(item, arg);
          builder.appendBoolean(r);
        }

        context.safepoint();
      }

      return builder.seal();
    }
  }

  protected Storage<Boolean> runLongMap(
      AbstractLongStorage lhs, long rhs, MapOperationProblemAggregator problemAggregator) {
    long n = lhs.getSize();
    Context context = Context.getCurrent();
    var builder = Builder.getForBoolean(n);
    for (long i = 0; i < n; ++i) {
      if (lhs.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        long item = lhs.getItemAsLong(i);
        boolean r = doLong(item, rhs);
        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Boolean> runDoubleMap(
      DoubleArrayAdapter lhs, double rhs, MapOperationProblemAggregator problemAggregator) {
    long n = lhs.size();
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; ++i) {
      if (lhs.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        double item = lhs.getItemAsDouble(i);
        boolean r = doDouble(item, rhs);
        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Boolean> runBigIntegerMap(
      BigIntegerArrayAdapter lhs, BigInteger rhs, MapOperationProblemAggregator problemAggregator) {
    long n = lhs.size();
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; ++i) {
      BigInteger item = lhs.getItem(i);
      if (item == null) {
        builder.appendNulls(1);
      } else {
        boolean r = doBigInteger(item, rhs);
        builder.appendBoolean(r);
      }
      context.safepoint();
    }
    return builder.seal();
  }

  protected Storage<Boolean> runBigDecimalMap(
      BigDecimalArrayAdapter lhs, BigDecimal rhs, MapOperationProblemAggregator problemAggregator) {
    long n = lhs.size();
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (int i = 0; i < n; ++i) {
      BigDecimal item = lhs.getItem(i);
      if (item == null) {
        builder.appendNulls(1);
      } else {
        builder.append(doBigDecimal(item, rhs));
      }

      context.safepoint();
    }

    return builder.seal();
  }

  @Override
  public Storage<Boolean> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    return switch (storage) {
      case DoubleStorage lhs -> switch (arg) {
        case BigDecimalStorage rhs -> runBigDecimalZip(
            BigDecimalArrayAdapter.fromStorage(lhs),
            BigDecimalArrayAdapter.fromBigDecimalStorage(rhs),
            problemAggregator);
        default -> {
          if (arg.getType() instanceof AnyObjectType) {
            yield runMixedZip(lhs, arg, problemAggregator);
          } else {
            yield runDoubleZip(lhs, DoubleArrayAdapter.fromAnyStorage(arg), problemAggregator);
          }
        }
      };

      case AbstractLongStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs -> runLongZip(lhs, rhs, problemAggregator);
        case BigIntegerStorage rhs -> {
          BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
          BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
          yield runBigIntegerZip(left, right, problemAggregator);
        }
        case BigDecimalStorage rhs -> runBigDecimalZip(
            BigDecimalArrayAdapter.fromStorage(lhs),
            BigDecimalArrayAdapter.fromBigDecimalStorage(rhs),
            problemAggregator);
        case DoubleStorage rhs -> runDoubleZip(
            DoubleArrayAdapter.fromStorage(lhs), rhs, problemAggregator);
        default -> runMixedZip(lhs, arg, problemAggregator);
      };

      case BigIntegerStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs -> {
          BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
          BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
          yield runBigIntegerZip(left, right, problemAggregator);
        }
        case BigIntegerStorage rhs -> {
          BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
          BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
          yield runBigIntegerZip(left, right, problemAggregator);
        }
        case BigDecimalStorage rhs -> runBigDecimalZip(
            BigDecimalArrayAdapter.fromBigIntegerStorage(lhs),
            BigDecimalArrayAdapter.fromBigDecimalStorage(rhs),
            problemAggregator);
        case DoubleStorage rhs -> runDoubleZip(
            DoubleArrayAdapter.fromBigIntegerStorage(lhs), rhs, problemAggregator);
        default -> runMixedZip(lhs, arg, problemAggregator);
      };

      case BigDecimalStorage lhs -> {
        if (arg instanceof AbstractLongStorage
            || arg instanceof BigIntegerStorage
            || arg instanceof BigDecimalStorage
            || arg instanceof DoubleStorage) {
          BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromAnyStorage(lhs);
          BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromAnyStorage(arg);
          yield runBigDecimalZip(left, right, problemAggregator);
        } else {
          yield runMixedZip(lhs, arg, problemAggregator);
        }
      }

      default -> throw new IllegalStateException(
          "Unsupported lhs storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected Storage<Boolean> runLongZip(
      AbstractLongStorage lhs,
      AbstractLongStorage rhs,
      MapOperationProblemAggregator problemAggregator) {
    long n = lhs.getSize();
    long m = Math.min(n, rhs.getSize());
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; ++i) {
      if (lhs.isNothing(i) || (i >= m || rhs.isNothing(i))) {
        builder.appendNulls(1);
      } else {
        long x = lhs.getItemAsLong(i);
        long y = rhs.getItemAsLong(i);
        boolean r = doLong(x, y);
        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Boolean> runDoubleZip(
      DoubleArrayAdapter lhs,
      DoubleArrayAdapter rhs,
      MapOperationProblemAggregator problemAggregator) {
    long n = lhs.size();
    long m = Math.min(n, rhs.size());
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; ++i) {
      if (lhs.isNothing(i) || (i >= m || rhs.isNothing(i))) {
        builder.appendNulls(1);
      } else {
        double x = lhs.getItemAsDouble(i);
        double y = rhs.getItemAsDouble(i);
        boolean r = doDouble(x, y);
        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Boolean> runBigIntegerZip(
      BigIntegerArrayAdapter lhs,
      BigIntegerArrayAdapter rhs,
      MapOperationProblemAggregator problemAggregator) {
    long n = lhs.size();
    long m = Math.min(n, rhs.size());
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; ++i) {
      BigInteger x = lhs.getItem(i);
      BigInteger y = i >= m ? null : rhs.getItem(i);
      if (x == null || y == null) {
        builder.appendNulls(1);
      } else {
        boolean r = doBigInteger(x, y);
        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Boolean> runBigDecimalZip(
      BigDecimalArrayAdapter lhs,
      BigDecimalArrayAdapter rhs,
      MapOperationProblemAggregator problemAggregator) {
    long n = lhs.size();
    long m = Math.min(n, rhs.size());
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (int i = 0; i < n; ++i) {
      BigDecimal x = lhs.getItem(i);
      BigDecimal y = i >= m ? null : rhs.getItem(i);
      if (x == null || y == null) {
        builder.appendNulls(1);
      } else {
        builder.appendBoolean(doBigDecimal(x, y));
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Boolean> runMixedZip(
      Storage<?> lhs, Storage<?> rhs, MapOperationProblemAggregator problemAggregator) {
    long n = lhs.getSize();
    long m = Math.min(n, rhs.getSize());
    var builder = Builder.getForBoolean(n);
    Context context = Context.getCurrent();
    for (long i = 0; i < n; ++i) {
      Object x = lhs.getItemBoxed(i);
      Object y = i >= m ? null : rhs.getItemBoxed(i);
      if (x == null || y == null) {
        builder.appendNulls(1);
      } else {
        boolean r;
        // Any number is coercible to double, if the value is not coercible, it is not a supported
        // number type.
        if (NumericConverter.isCoercibleToDouble(x) && NumericConverter.isCoercibleToDouble(y)) {

          // If any of the values is decimal like, then decimal type is used for comparison.
          if (NumericConverter.isFloatLike(x) || NumericConverter.isFloatLike(y)) {
            double a = NumericConverter.coerceToDouble(x);
            double b = NumericConverter.coerceToDouble(y);
            r = doDouble(a, b);
          } else {
            if (x instanceof BigInteger || y instanceof BigInteger) {
              BigInteger a = NumericConverter.coerceToBigInteger(x);
              BigInteger b = NumericConverter.coerceToBigInteger(y);
              r = doBigInteger(a, b);
            } else {
              long a = NumericConverter.coerceToLong(x);
              long b = NumericConverter.coerceToLong(y);
              r = doLong(a, b);
            }
          }
        } else {
          r = onOtherType(x, y);
        }

        builder.appendBoolean(r);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
