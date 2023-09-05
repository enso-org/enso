package org.enso.table.data.column.operation.map.numeric.comparisons;

import org.enso.base.CompareException;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.numeric.helpers.BigIntegerArrayAdapter;
import org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.util.BitSets;
import org.graalvm.polyglot.Context;

import java.math.BigInteger;
import java.util.BitSet;

import static org.enso.table.data.column.operation.map.numeric.helpers.DoubleArrayAdapter.fromAnyStorage;

public abstract class NumericComparison<T extends Number, I extends Storage<? super T>> extends BinaryMapOperation<T,
    I> {

  protected abstract boolean doDouble(double a, double b);

  protected abstract boolean doLong(long a, long b);

  protected abstract boolean doBigInteger(BigInteger a, BigInteger b);

  protected boolean onOtherType(Object a, Object b) {
    throw new CompareException(a, b);
  }

  public NumericComparison(String name) {
    super(name);
  }

  @Override
  public BoolStorage runBinaryMap(I storage, Object arg, MapOperationProblemBuilder problemBuilder) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.size());
    } else if (arg instanceof BigInteger bigInteger) {
      return switch (storage) {
        case AbstractLongStorage s ->
            runBigIntegerMap(BigIntegerArrayAdapter.fromStorage(s), bigInteger, problemBuilder);
        case BigIntegerStorage s -> runBigIntegerMap(BigIntegerArrayAdapter.fromStorage(s), bigInteger, problemBuilder);
        case DoubleStorage s -> runDoubleMap(s, bigInteger.doubleValue(), problemBuilder);
        default -> throw new IllegalStateException("Unsupported lhs storage: " + storage.getClass().getCanonicalName());
      };
    } else if (NumericConverter.isCoercibleToLong(arg)) {
      long rhs = NumericConverter.coerceToLong(arg);
      return switch (storage) {
        case AbstractLongStorage s -> runLongMap(s, rhs, problemBuilder);
        case BigIntegerStorage s ->
            runBigIntegerMap(BigIntegerArrayAdapter.fromStorage(s), BigInteger.valueOf(rhs), problemBuilder);
        case DoubleStorage s -> runDoubleMap(s, (double) rhs, problemBuilder);
        default -> throw new IllegalStateException("Unsupported lhs storage: " + storage.getClass().getCanonicalName());
      };
    } else if (NumericConverter.isCoercibleToDouble(arg)) {
      DoubleArrayAdapter lhs = DoubleArrayAdapter.fromAnyStorage(storage);
      double rhs = NumericConverter.coerceToDouble(arg);
      return runDoubleMap(lhs, rhs, problemBuilder);
    } else {
      int n = storage.size();
      BitSet missing = new BitSet();
      BitSet comparisonResults = new BitSet();
      Context context = Context.getCurrent();
      for (int i = 0; i < n; ++i) {
        Object item = storage.getItemBoxed(i);
        if (item == null) {
          missing.set(i);
        } else {
          boolean r = onOtherType(item, arg);
          if (r) {
            comparisonResults.set(i);
          }
        }

        context.safepoint();
      }

      return new BoolStorage(comparisonResults, missing, n, false);
    }
  }

  protected BoolStorage runLongMap(AbstractLongStorage lhs, long rhs, MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    BitSet comparisonResults = new BitSet();
    BitSet missing = BitSets.makeDuplicate(lhs.getIsMissing());
    Context context = Context.getCurrent();
    for (int i = 0; i < n; ++i) {
      if (!lhs.isNa(i)) {
        long item = lhs.getItem(i);
        boolean r = doLong(item, rhs);
        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }

  protected BoolStorage runDoubleMap(DoubleArrayAdapter lhs, double rhs, MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    BitSet comparisonResults = new BitSet();
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < n; ++i) {
      if (lhs.isNa(i)) {
        missing.set(i);
      } else {
        double item = lhs.getItemAsDouble(i);
        boolean r = doDouble(item, rhs);
        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }

  protected BoolStorage runBigIntegerMap(BigIntegerArrayAdapter lhs, BigInteger rhs,
                                         MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    BitSet comparisonResults = new BitSet();
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < n; ++i) {
      BigInteger item = lhs.getItem(i);
      if (item == null) {
        missing.set(i);
      } else {
        boolean r = doBigInteger(item, rhs);
        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }

  @Override
  public BoolStorage runZip(I storage, Storage<?> arg, MapOperationProblemBuilder problemBuilder) {
    return switch (storage) {
      case DoubleStorage lhs -> {
        if (arg.getType() instanceof AnyObjectType) {
          yield runMixedZip(lhs, arg, problemBuilder);
        } else {
          yield runDoubleZip(lhs, fromAnyStorage(arg), problemBuilder);
        }
      }

      case AbstractLongStorage lhs -> switch (arg) {
        case AbstractLongStorage rhs -> runLongZip(lhs, rhs, problemBuilder);
        case BigIntegerStorage rhs -> {
          BigIntegerArrayAdapter left = BigIntegerArrayAdapter.fromStorage(lhs);
          BigIntegerArrayAdapter right = BigIntegerArrayAdapter.fromStorage(rhs);
          yield runBigIntegerZip(left, right, problemBuilder);
        }
        case DoubleStorage rhs -> runDoubleZip(DoubleArrayAdapter.fromStorage(lhs), rhs, problemBuilder);
        case default -> runMixedZip(lhs, arg, problemBuilder);
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
          case default -> runMixedZip(lhs, arg, problemBuilder);
        };
      }

      case default ->
          throw new IllegalStateException("Unsupported lhs storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected BoolStorage runLongZip(AbstractLongStorage lhs, AbstractLongStorage rhs,
                                   MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    int m = Math.min(lhs.size(), rhs.size());
    BitSet comparisonResults = new BitSet();
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < m; ++i) {
      if (lhs.isNa(i) || rhs.isNa(i)) {
        missing.set(i);
      } else {
        long x = lhs.getItem(i);
        long y = rhs.getItem(i);
        boolean r = doLong(x, y);
        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    if (m < n) {
      missing.set(m, n);
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }

  protected BoolStorage runDoubleZip(DoubleArrayAdapter lhs, DoubleArrayAdapter rhs,
                                     MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    int m = Math.min(lhs.size(), rhs.size());
    BitSet comparisonResults = new BitSet();
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < m; ++i) {
      if (lhs.isNa(i) || rhs.isNa(i)) {
        missing.set(i);
      } else {
        double x = lhs.getItemAsDouble(i);
        double y = rhs.getItemAsDouble(i);
        boolean r = doDouble(x, y);
        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    if (m < n) {
      missing.set(m, n);
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }

  protected BoolStorage runBigIntegerZip(BigIntegerArrayAdapter lhs, BigIntegerArrayAdapter rhs,
                                         MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    int m = Math.min(lhs.size(), rhs.size());
    BitSet comparisonResults = new BitSet();
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < m; ++i) {
      BigInteger x = lhs.getItem(i);
      BigInteger y = rhs.getItem(i);
      if (x == null || y == null) {
        missing.set(i);
      } else {
        boolean r = doBigInteger(x, y);
        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    if (m < n) {
      missing.set(m, n);
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }

  protected BoolStorage runMixedZip(Storage<?> lhs, Storage<?> rhs, MapOperationProblemBuilder problemBuilder) {
    int n = lhs.size();
    int m = Math.min(lhs.size(), rhs.size());
    BitSet comparisonResults = new BitSet();
    BitSet missing = new BitSet();
    Context context = Context.getCurrent();
    for (int i = 0; i < m; ++i) {
      Object x = lhs.getItemBoxed(i);
      Object y = rhs.getItemBoxed(i);
      if (x == null || y == null) {
        missing.set(i);
      } else {
        boolean r = false;
        // Any number is coercible to double, if the value is not coercible, it is not a supported number type.
        if (NumericConverter.isCoercibleToDouble(x) && NumericConverter.isCoercibleToDouble(y)) {

          // If any of the values is decimal like, then decimal type is used for comparison.
          if (NumericConverter.isDecimalLike(x) || NumericConverter.isDecimalLike(y)) {
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

        if (r) {
          comparisonResults.set(i);
        }
      }

      context.safepoint();
    }

    if (m < n) {
      missing.set(m, n);
    }

    return new BoolStorage(comparisonResults, missing, n, false);
  }
}
