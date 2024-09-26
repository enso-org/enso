package org.enso.table.data.column.operation.map.numeric.arithmetic;

import static org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter.fromAnyStorage;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.BitSet;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
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
              BigDecimalArrayAdapter.fromStorage(s), new BigDecimal(rhs), problemAggregator);
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
              BigDecimalArrayAdapter.fromStorage(s),
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
              DoubleArrayAdapter.fromStorage(s), doubleArg, problemAggregator);
          case BigDecimalStorage s -> runBigDecimalMap(
              BigDecimalArrayAdapter.fromStorage(s),
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
          BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromStorage(rhs);
          yield runBigDecimalZip(left, right, problemAggregator);
        }
        default -> runDoubleZip(lhs, fromAnyStorage(arg), problemAggregator);
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
          BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromStorage(rhs);
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
              DoubleArrayAdapter.fromStorage(lhs), rhs, problemAggregator);
          case BigDecimalStorage rhs -> {
            BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(lhs);
            BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromStorage(rhs);
            yield runBigDecimalZip(left, right, problemAggregator);
          }
          default -> throw new IllegalStateException(
              "Unsupported storage: " + arg.getClass().getCanonicalName());
        };
      }

      case BigDecimalStorage lhs -> {
        BigDecimalArrayAdapter left = BigDecimalArrayAdapter.fromStorage(lhs);
        BigDecimalArrayAdapter right = BigDecimalArrayAdapter.fromAnyStorage(arg);
        yield runBigDecimalZip(left, right, problemAggregator);
      }

      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected DoubleStorage runDoubleZip(
      DoubleArrayAdapter a, DoubleArrayAdapter b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < m; i++) {
      if (a.isNothing(i) || b.isNothing(i)) {
        isNothing.set(i);
      } else {
        double r = doDouble(a.getItemAsDouble(i), b.getItemAsDouble(i), i, problemAggregator);
        out[i] = Double.doubleToRawLongBits(r);
      }

      context.safepoint();
    }

    if (m < n) {
      isNothing.set(m, n);
    }

    return new DoubleStorage(out, n, isNothing);
  }

  private static Storage<? extends Number> allNullStorageOfSameType(Storage<?> storage) {
    return switch (storage) {
      case AbstractLongStorage s -> LongStorage.makeEmpty(storage.size(), INTEGER_RESULT_TYPE);
      case BigIntegerStorage s -> BigIntegerStorage.makeEmpty(storage.size());
      case DoubleStorage s -> DoubleStorage.makeEmpty(storage.size());
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected DoubleStorage runDoubleMap(
      DoubleArrayAdapter a, Double b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return DoubleStorage.makeEmpty(a.size());
    }

    double bNonNull = b;
    Context context = Context.getCurrent();
    int n = a.size();
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (a.isNothing(i)) {
        isNothing.set(i);
      } else {
        double r = doDouble(a.getItemAsDouble(i), bNonNull, i, problemAggregator);
        out[i] = Double.doubleToRawLongBits(r);
      }

      context.safepoint();
    }

    return new DoubleStorage(out, n, isNothing);
  }

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
      if (a.isNothing(i) || b.isNothing(i)) {
        isNothing.set(i);
      } else {
        Long r = doLong(a.getItem(i), b.getItem(i), i, problemAggregator);
        if (r == null) {
          isNothing.set(i);
        } else {
          out[i] = r;
        }
      }

      context.safepoint();
    }

    if (m < n) {
      isNothing.set(m, n);
    }

    return new LongStorage(out, n, isNothing, INTEGER_RESULT_TYPE);
  }

  protected Storage<Long> runLongMap(
      AbstractLongStorage a, Long b, MapOperationProblemAggregator problemAggregator) {
    if (b == null) {
      return LongStorage.makeEmpty(a.size(), INTEGER_RESULT_TYPE);
    }

    long bNonNull = b;
    Context context = Context.getCurrent();
    int n = a.size();
    long[] out = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (a.isNothing(i)) {
        isNothing.set(i);
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
      if (x != null && y != null) {
        BigInteger r = doBigInteger(x, y, i, problemAggregator);
        out[i] = r;
      }
      context.safepoint();
    }

    return new BigIntegerStorage(out, n);
  }

  protected BigIntegerStorage runBigIntegerMap(
      BigIntegerArrayAdapter a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    BigInteger[] out = new BigInteger[n];
    for (int i = 0; i < n; i++) {
      BigInteger x = a.getItem(i);
      if (x == null || b == null) {
        out[i] = null;
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
      if (x != null && y != null) {
        BigDecimal r = doBigDecimal(x, y, i, problemAggregator);
        out[i] = r;
      }
      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }

  protected SpecializedStorage<BigDecimal> runBigDecimalMap(
      BigDecimalArrayAdapter a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    Context context = Context.getCurrent();
    int n = a.size();
    BigDecimal[] out = new BigDecimal[n];
    for (int i = 0; i < n; i++) {
      BigDecimal x = a.getItem(i);
      if (x == null || b == null) {
        out[i] = null;
      } else {
        BigDecimal r = doBigDecimal(x, b, i, problemAggregator);
        out[i] = r;
      }

      context.safepoint();
    }

    return new BigDecimalStorage(out, n);
  }
}
