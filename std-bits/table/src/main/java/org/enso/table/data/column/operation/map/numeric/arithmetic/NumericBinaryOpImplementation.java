package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.numeric.helpers.BigDecimalArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.BigIntegerArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

/** An operation expecting a numeric argument and returning a numeric column. */
public abstract class NumericBinaryOpImplementation<T extends Number, I extends Storage<? super T>>
    extends BinaryMapOperation<T, I> implements NumericBinaryOpDefinition {

  // The type to use for small integer results (regardless of the input bit size).
  public static final IntegerType INTEGER_RESULT_TYPE = IntegerType.INT_64;

  public NumericBinaryOpImplementation(String name) {
    super(name);
  }

  @Override
  public Storage<? extends Number> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return allNullStorageOfSameType(storage);
    } else {
      if (arg instanceof BigInteger rhs) {
        return switch (storage) {
          case AbstractLongStorage s -> runBigIntegerMap(
              BigIntegerArrayAdapter.fromStorage(s), rhs, problemAggregator);
          case BigIntegerStorage s -> runBigIntegerMap(
              BigIntegerArrayAdapter.fromStorage(s), rhs, problemAggregator);
          case BigDecimalStorage s -> runBigDecimalMap(
              BigDecimalArrayAdapter.fromBigDecimalStorage(s),
              new BigDecimal(rhs),
              problemAggregator);
          case DoubleStorage s -> runDoubleMap(s, rhs.doubleValue(), problemAggregator);
          default -> throw new IllegalStateException(
              "Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (NumericConverter.isCoercibleToLong(arg)) {
        long argAsLong = NumericConverter.coerceToLong(arg);
        return switch (storage) {
          case AbstractLongStorage s -> runLongMap(s, argAsLong, problemAggregator);
          case BigIntegerStorage s -> runBigIntegerMap(
              BigIntegerArrayAdapter.fromStorage(s),
              BigInteger.valueOf(argAsLong),
              problemAggregator);
          case BigDecimalStorage s -> runBigDecimalMap(
              BigDecimalArrayAdapter.fromBigDecimalStorage(s),
              BigDecimal.valueOf(argAsLong),
              problemAggregator);
          case DoubleStorage s -> runDoubleMap(s, (double) argAsLong, problemAggregator);
          default -> throw new IllegalStateException(
              "Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (NumericConverter.isCoercibleToDouble(arg)) {
        double doubleArg = NumericConverter.coerceToDouble(arg);
        return switch (storage) {
          case AbstractLongStorage s -> runDoubleMap(
              DoubleArrayAdapter.fromStorage(s), doubleArg, problemAggregator);
          case BigIntegerStorage s -> runDoubleMap(
              DoubleArrayAdapter.fromBigIntegerStorage(s), doubleArg, problemAggregator);
          case BigDecimalStorage s -> runBigDecimalMap(
              BigDecimalArrayAdapter.fromBigDecimalStorage(s),
              BigDecimal.valueOf(doubleArg),
              problemAggregator);
          case DoubleStorage s -> runDoubleMap(s, doubleArg, problemAggregator);
          default -> throw new IllegalStateException(
              "Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (arg instanceof BigDecimal bd) {
        return runBigDecimalMap(
            BigDecimalArrayAdapter.fromAnyStorage(storage), bd, problemAggregator);
      } else {
        throw new UnexpectedTypeException("a Number.");
      }
    }
  }

  @Override
  public Storage<? extends Number> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    return switch (storage) {
      case DoubleStorage lhs -> switch (arg) {
        case BigDecimalStorage rhs -> {
          BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(lhs);
          BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromBigDecimalStorage(rhs);
          yield runBigDecimalZip(left, right, problemAggregator);
        }
        default -> runDoubleZip(lhs, DoubleArrayAdapter.fromAnyStorage(arg), problemAggregator);
      };

      case AbstractLongStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs -> runLongZip(lhs, rhs, problemAggregator);
        case BigIntegerStorage rhs -> {
          BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
          BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
          yield runBigIntegerZip(left, right, problemAggregator);
        }
        case DoubleStorage rhs -> runDoubleZip(
            DoubleArrayAdapter.fromStorage(lhs), rhs, problemAggregator);
        case BigDecimalStorage rhs -> {
          BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(lhs);
          BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromBigDecimalStorage(rhs);
          yield runBigDecimalZip(left, right, problemAggregator);
        }
        default -> throw new IllegalStateException(
            "Unsupported storage: " + arg.getClass().getCanonicalName());
      };

      case BigIntegerStorage lhs -> {
        yield switch (arg) {
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
          case DoubleStorage rhs -> runDoubleZip(
              DoubleArrayAdapter.fromBigIntegerStorage(lhs), rhs, problemAggregator);
          case BigDecimalStorage rhs -> {
            BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromBigIntegerStorage(lhs);
            BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromBigDecimalStorage(rhs);
            yield runBigDecimalZip(left, right, problemAggregator);
          }
          default -> throw new IllegalStateException(
              "Unsupported storage: " + arg.getClass().getCanonicalName());
        };
      }

      case BigDecimalStorage lhs -> {
        BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromBigDecimalStorage(lhs);
        BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromAnyStorage(arg);
        yield runBigDecimalZip(left, right, problemAggregator);
      }

      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected Storage<Double> runDoubleZip(
      DoubleArrayAdapter a, DoubleArrayAdapter b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    long n = a.size();
    long m = Math.min(n, b.size());
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    for (long i = 0; i < m; i++) {
      if (a.isNothing(i) || b.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        builder.append(doDouble(a.getItemAsDouble(i), b.getItemAsDouble(i), i, problemAggregator));
      }

      context.safepoint();
    }

    if (m < n) {
      builder.appendNulls(Math.toIntExact(n - m));
    }

    return builder.seal();
  }

  private static Storage<? extends Number> allNullStorageOfSameType(Storage<?> storage) {
    return switch (storage) {
      case AbstractLongStorage s -> LongStorage.makeEmpty(storage.getSize(), INTEGER_RESULT_TYPE);
      case BigIntegerStorage s -> BigIntegerStorage.makeEmpty(storage.getSize());
      case DoubleStorage s -> DoubleStorage.makeEmpty(storage.getSize());
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected Storage<Double> runDoubleMap(
      DoubleArrayAdapter a, Double b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return DoubleStorage.makeEmpty(a.size());
    }

    double bNonNull = b;
    Context context = Context.getCurrent();
    long n = a.size();
    var builder = Builder.getForDouble(FloatType.FLOAT_64, n, problemAggregator);
    for (long i = 0; i < n; i++) {
      if (a.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        builder.appendDouble(doDouble(a.getItemAsDouble(i), bNonNull, i, problemAggregator));
      }

      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<Long> runLongZip(
      AbstractLongStorage a,
      AbstractLongStorage b,
      MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    long n = a.getSize();
    long m = Math.min(n, b.getSize());
    var builder = Builder.getForLong(INTEGER_RESULT_TYPE, n, problemAggregator);
    for (long i = 0; i < n; i++) {
      if (a.isNothing(i) || i >= m || b.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        Long r = doLong(a.getItemAsLong(i), b.getItemAsLong(i), i, problemAggregator);
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

  protected Storage<Long> runLongMap(
      AbstractLongStorage a, Long b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return LongStorage.makeEmpty(a.getSize(), INTEGER_RESULT_TYPE);
    }

    long bNonNull = b;
    Context context = Context.getCurrent();
    long n = a.getSize();
    var builder = Builder.getForLong(INTEGER_RESULT_TYPE, n, problemAggregator);
    for (long i = 0; i < n; i++) {
      if (a.isNothing(i)) {
        builder.appendNulls(1);
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
    Context context = Context.getCurrent();
    long n = a.size();
    long m = Math.min(n, b.size());
    var builder = Builder.getForBigInteger(n, problemAggregator);
    for (long i = 0; i < m; i++) {
      BigInteger x = a.getItem(i);
      BigInteger y = b.getItem(i);
      if (x != null && y != null) {
        builder.append(doBigInteger(x, y, i, problemAggregator));
      } else {
        builder.appendNulls(1);
      }
      context.safepoint();
    }

    if (m < n) {
      builder.appendNulls(Math.toIntExact(n - m));
    }

    return builder.seal();
  }

  protected Storage<BigInteger> runBigIntegerMap(
      BigIntegerArrayAdapter a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    long n = a.size();
    var builder = Builder.getForBigInteger(n, problemAggregator);
    for (long i = 0; i < n; i++) {
      BigInteger x = a.getItem(i);
      if (x == null || b == null) {
        builder.appendNulls(1);
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
    Context context = Context.getCurrent();
    long n = a.size();
    long m = Math.min(a.size(), b.size());
    var builder = Builder.getForBigDecimal(n);
    for (long i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      BigDecimal y = i >= m ? null : b.getItem(i);
      if (x != null && y != null) {
        builder.append(doBigDecimal(x, y, i, problemAggregator));
      } else {
        builder.appendNulls(1);
      }
      context.safepoint();
    }

    return builder.seal();
  }

  protected Storage<BigDecimal> runBigDecimalMap(
      BigDecimalArrayAdapter a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    long n = a.size();
    var builder = Builder.getForBigDecimal(n);

    for (long i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      if (x == null || b == null) {
        builder.appendNulls(1);
      } else {
        builder.append(doBigDecimal(x, b, i, problemAggregator));
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
