package org.enso.table.data.column.operation.map.numeric;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
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

/**
 * An operation expecting a numeric argument and returning a numeric column.
 */
public abstract class NumericBinaryOpImplementation<T extends Number, I extends Storage<? super T>> extends BinaryMapOperation<T, I> implements NumericBinaryOpDefinition {
  // Regardless of input type, our integer operations return 64-bit integers.
  private static final IntegerType INTEGER_RESULT_TYPE = IntegerType.INT_64;

  public NumericBinaryOpImplementation(String name) {
    super(name);
  }

  public interface DoubleArrayAdapter {
    double getItemAsDouble(int i);

    boolean isNa(int i);

    int size();
  }

  public interface BigIntegerArrayAdapter {
    BigInteger getItem(int i);

    int size();
  }

  protected static class LongStorageAsDouble implements DoubleArrayAdapter {
    private final AbstractLongStorage storage;

    LongStorageAsDouble(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(int i) {
      long x = storage.getItem(i);
      return (double) x;
    }

    @Override
    public boolean isNa(int i) {
      return storage.isNa(i);
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  protected static class BigIntegerStorageAsDouble implements DoubleArrayAdapter {
    private final BigIntegerStorage storage;

    BigIntegerStorageAsDouble(BigIntegerStorage storage) {
      this.storage = storage;
    }

    @Override
    public double getItemAsDouble(int i) {
      BigInteger x = storage.getItem(i);
      return x.doubleValue();
    }

    @Override
    public boolean isNa(int i) {
      return storage.getItem(i) == null;
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  protected static class BigIntegerStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final BigIntegerStorage storage;

    BigIntegerStorageAsBigInteger(BigIntegerStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(int i) {
      return storage.getItemBoxed(i);
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  protected static class LongStorageAsBigInteger implements BigIntegerArrayAdapter {
    private final AbstractLongStorage storage;

    LongStorageAsBigInteger(AbstractLongStorage storage) {
      this.storage = storage;
    }

    @Override
    public BigInteger getItem(int i) {
      if (storage.isNa(i)) {
        return null;
      } else {
        long x = storage.getItem(i);
        return BigInteger.valueOf(x);
      }
    }

    @Override
    public int size() {
      return storage.size();
    }
  }

  @Override
  public Storage<? extends Number> runBinaryMap(I storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      return switch (storage) {
        case AbstractLongStorage s -> LongStorage.makeEmpty(storage.size(), INTEGER_RESULT_TYPE);
        case BigIntegerStorage s -> BigIntegerStorage.makeEmpty(storage.size());
        case DoubleStorage s -> DoubleStorage.makeEmpty(storage.size());
        case default ->
            throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
      };
    } else {
      if (arg instanceof BigInteger rhs) {
        return switch (storage) {
          case AbstractLongStorage s -> runBigIntegerMap(new LongStorageAsBigInteger(s), rhs, problemBuilder);
          case BigIntegerStorage s -> runBigIntegerMap(new BigIntegerStorageAsBigInteger(s), rhs, problemBuilder);
          case DoubleStorage s -> runDoubleMap(s, rhs.doubleValue(), problemBuilder);
          case default ->
              throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (NumericConverter.isCoercibleToLong(arg)) {
        long argAsLong = NumericConverter.coerceToLong(arg);
        return switch (storage) {
          case AbstractLongStorage s -> runLongMap(s, argAsLong, problemBuilder);
          case BigIntegerStorage s ->
              runBigIntegerMap(new BigIntegerStorageAsBigInteger(s), BigInteger.valueOf(argAsLong), problemBuilder);
          case DoubleStorage s -> runDoubleMap(s, (double) argAsLong, problemBuilder);
          case default ->
              throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
        };
      } else if (NumericConverter.isCoercibleToDouble(arg)) {
        double doubleArg = NumericConverter.coerceToDouble(arg);
        return switch (storage) {
          case AbstractLongStorage s -> runDoubleMap(new LongStorageAsDouble(s), doubleArg, problemBuilder);
          case BigIntegerStorage s -> runDoubleMap(new BigIntegerStorageAsDouble(s), doubleArg, problemBuilder);
          case DoubleStorage s -> runDoubleMap(s, doubleArg, problemBuilder);
          case default ->
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
      case DoubleStorage lhs ->
        runDoubleZip(lhs, numericStorageAsDouble(arg), problemBuilder);
      case AbstractLongStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs -> runLongZip(lhs, rhs, problemBuilder);
        case BigIntegerStorage rhs ->
            runBigIntegerZip(new LongStorageAsBigInteger(lhs), new BigIntegerStorageAsBigInteger(rhs), problemBuilder);
        case DoubleStorage rhs -> runDoubleZip(new LongStorageAsDouble(lhs), rhs, problemBuilder);
        case default -> throw new IllegalStateException("Unsupported storage: " + arg.getClass().getCanonicalName());
      };
      case BigIntegerStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs ->
            runBigIntegerZip(new LongStorageAsBigInteger(rhs), new BigIntegerStorageAsBigInteger(lhs), problemBuilder);
        case BigIntegerStorage rhs ->
            runBigIntegerZip(new BigIntegerStorageAsBigInteger(lhs), new BigIntegerStorageAsBigInteger(rhs),
                problemBuilder);
        case DoubleStorage rhs -> runDoubleZip(new BigIntegerStorageAsDouble(lhs), rhs, problemBuilder);
        case default -> throw new IllegalStateException("Unsupported storage: " + arg.getClass().getCanonicalName());
      };
      case default -> throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected DoubleArrayAdapter numericStorageAsDouble(Storage<?> storage) {
    return switch (storage) {
      case DoubleStorage s -> s;
      case AbstractLongStorage s -> new LongStorageAsDouble(s);
      case BigIntegerStorage s -> new BigIntegerStorageAsDouble(s);
      case default -> throw new IllegalStateException("Unsupported storage: " + storage.getClass().getCanonicalName());
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
