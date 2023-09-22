package org.enso.table.data.column.operation.map.numeric.arithmetic;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.numeric.helpers.BigIntegerArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

import java.math.BigInteger;
import java.util.BitSet;

import static org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter.fromAnyStorage;

/**
 * An operation expecting a numeric argument and returning a numeric column.
 */
public abstract class NumericBinaryOpImplementation<T extends Number, I extends Storage<? super T>> extends BinaryMapOperation<T, I> implements NumericBinaryOpDefinition {

  // The type to use for small integer results (regardless of the input bit size).
  public static final IntegerType INTEGER_RESULT_TYPE = IntegerType.INT_64;

  public NumericBinaryOpImplementation(String name) {
    super(name);
  }

  @Override
  public Storage<? extends Number> runBinaryMap(I storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      return allNullStorageOfSameType(storage);
    } else {
      if (arg instanceof BigInteger rhs) {
        return switch (storage) {
          case AbstractLongStorage s -> runBigIntegerMap(BigIntegerArrayAdapter.fromStorage(s), rhs, problemBuilder);
          case BigIntegerStorage s -> runBigIntegerMap(BigIntegerArrayAdapter.fromStorage(s), rhs, problemBuilder);
          case DoubleStorage s -> runDoubleMap(s, rhs.doubleValue(), problemBuilder);
          default ->
              throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (NumericConverter.isCoercibleToLong(arg)) {
        long argAsLong = NumericConverter.coerceToLong(arg);
        return switch (storage) {
          case AbstractLongStorage s -> runLongMap(s, argAsLong, problemBuilder);
          case BigIntegerStorage s ->
              runBigIntegerMap(BigIntegerArrayAdapter.fromStorage(s), BigInteger.valueOf(argAsLong), problemBuilder);
          case DoubleStorage s -> runDoubleMap(s, (double) argAsLong, problemBuilder);
          default ->
              throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (NumericConverter.isCoercibleToDouble(arg)) {
        double doubleArg = NumericConverter.coerceToDouble(arg);
        return switch (storage) {
          case AbstractLongStorage s -> runDoubleMap(DoubleArrayAdapter.fromStorage(s), doubleArg, problemBuilder);
          case BigIntegerStorage s -> runDoubleMap(DoubleArrayAdapter.fromStorage(s), doubleArg, problemBuilder);
          case DoubleStorage s -> runDoubleMap(s, doubleArg, problemBuilder);
          default ->
              throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else {
        throw new UnexpectedTypeException("a Number.");
      }
    }
  }

  @Override
  public Storage<? extends Number> runZip(I storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    return switch (storage) {
      case DoubleStorage lhs -> runDoubleZip(lhs, fromAnyStorage(arg), problemBuilder);

      case AbstractLongStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs -> runLongZip(lhs, rhs, problemBuilder);
        case BigIntegerStorage rhs -> {
          BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
          BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
          yield runBigIntegerZip(left, right, problemBuilder);
        }
        case DoubleStorage rhs -> runDoubleZip(DoubleArrayAdapter.fromStorage(lhs), rhs, problemBuilder);
        default -> throw new IllegalStateException("Unsupported storage: " + arg.getClass().getCanonicalName());
      };

      case BigIntegerStorage lhs -> {
        BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
        yield switch (arg) {
          case AbstractLongStorage rhs -> {
            BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
            yield runBigIntegerZip(left, right, problemBuilder);
          }
          case BigIntegerStorage rhs -> {
            BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
            yield runBigIntegerZip(left, right, problemBuilder);
          }
          case DoubleStorage rhs -> runDoubleZip(DoubleArrayAdapter.fromStorage(lhs), rhs, problemBuilder);
          default -> throw new IllegalStateException("Unsupported storage: " + arg.getClass().getCanonicalName());
        };
      }

      default -> throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected DoubleStorage runDoubleZip(DoubleArrayAdapter a, DoubleArrayAdapter b,
                                       MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    long[] out = new long[n];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < m; i++) {
      if (a.isNa(i) || b.isNa(i)) {
        newMissing.set(i);
      } else {
        double r = doDouble(a.getItemAsDouble(i), b.getItemAsDouble(i), i, problemBuilder);
        out[i] = Double.doubleToRawLongBits(r);
      }

      context.safepoint();
    }

    if (m < n) {
      newMissing.set(m, n);
    }

    return new DoubleStorage(out, n, newMissing);
  }

  private static Storage<? extends Number> allNullStorageOfSameType(Storage<?> storage) {
    return switch (storage) {
      case AbstractLongStorage s -> LongStorage.makeEmpty(storage.size(), INTEGER_RESULT_TYPE);
      case BigIntegerStorage s -> BigIntegerStorage.makeEmpty(storage.size());
      case DoubleStorage s -> DoubleStorage.makeEmpty(storage.size());
      default -> throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected DoubleStorage runDoubleMap(DoubleArrayAdapter a, Double b, MapOperationProblemBuilder problemBuilder) {
    if (b == null) {
      return DoubleStorage.makeEmpty(a.size());
    }

    double bNonNull = b;
    Context context = Context.getCurrent();
    int n = a.size();
    long[] out = new long[n];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (a.isNa(i)) {
        newMissing.set(i);
      } else {
        double r = doDouble(a.getItemAsDouble(i), bNonNull, i, problemBuilder);
        out[i] = Double.doubleToRawLongBits(r);
      }

      context.safepoint();
    }

    return new DoubleStorage(out, n, newMissing);
  }

  protected LongStorage runLongZip(AbstractLongStorage a, AbstractLongStorage b,
                                   MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    long[] out = new long[n];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < m; i++) {
      if (a.isNa(i) || b.isNa(i)) {
        newMissing.set(i);
      } else {
        Long r = doLong(a.getItem(i), b.getItem(i), i, problemBuilder);
        if (r == null) {
          newMissing.set(i);
        } else {
          out[i] = r;
        }
      }

      context.safepoint();
    }

    if (m < n) {
      newMissing.set(m, n);
    }

    return new LongStorage(out, n, newMissing, INTEGER_RESULT_TYPE);
  }

  protected LongStorage runLongMap(AbstractLongStorage a, Long b, MapOperationProblemBuilder problemBuilder) {
    if (b == null) {
      return LongStorage.makeEmpty(a.size(), INTEGER_RESULT_TYPE);
    }

    long bNonNull = b;
    Context context = Context.getCurrent();
    int n = a.size();
    long[] out = new long[n];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (a.isNa(i)) {
        newMissing.set(i);
      } else {
        Long r = doLong(a.getItem(i), bNonNull, i, problemBuilder);
        if (r == null) {
          newMissing.set(i);
        } else {
          out[i] = r;
        }
      }

      context.safepoint();
    }

    return new LongStorage(out, n, newMissing, INTEGER_RESULT_TYPE);
  }

  protected BigIntegerStorage runBigIntegerZip(BigIntegerArrayAdapter a, BigIntegerArrayAdapter b,
                                               MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    int n = a.size();
    int m = Math.min(a.size(), b.size());
    BigInteger[] out = new BigInteger[n];
    BitSet newMissing = new BitSet();
    for (int i = 0; i < m; i++) {
      BigInteger x = a.getItem(i);
      BigInteger y = b.getItem(i);
      if (x == null || y == null) {
        newMissing.set(i);
      } else {
        BigInteger r = doBigInteger(x, y, i, problemBuilder);
        out[i] = r;
      }

      context.safepoint();
    }

    if (m < n) {
      newMissing.set(m, n);
    }

    return new BigIntegerStorage(out, n);
  }

  protected BigIntegerStorage runBigIntegerMap(BigIntegerArrayAdapter a, BigInteger b,
                                               MapOperationProblemBuilder problemBuilder) {
    Context context = Context.getCurrent();
    int n = a.size();
    BigInteger[] out = new BigInteger[n];
    for (int i = 0; i < n; i++) {
      BigInteger x = a.getItem(i);
      if (x == null || b == null) {
        out[i] = null;
      } else {
        BigInteger r = doBigInteger(x, b, i, problemBuilder);
        out[i] = r;
      }

      context.safepoint();
    }

    return new BigIntegerStorage(out, n);
  }
}
