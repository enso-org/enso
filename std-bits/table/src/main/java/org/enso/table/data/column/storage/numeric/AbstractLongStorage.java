package org.enso.table.data.column.storage.numeric;

import java.util.BitSet;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.numeric.LongBooleanOp;
import org.enso.table.data.column.operation.map.numeric.LongComparison;
import org.enso.table.data.column.operation.map.numeric.LongIsInOp;
import org.enso.table.data.column.operation.map.numeric.LongNumericOp;
import org.enso.table.data.column.operation.map.numeric.LongRoundOp;
import org.enso.table.data.column.operation.map.numeric.UnaryLongToLongOp;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.graalvm.polyglot.Context;

public abstract class AbstractLongStorage extends NumericStorage<Long> {
  public abstract long getItem(int idx);

  public abstract BitSet getIsMissing();

  private static final MapOperationStorage<Long, AbstractLongStorage> ops = buildOps();

  @Override
  public boolean isUnaryOpVectorized(String name) {
    return ops.isSupportedUnary(name);
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(String name, MapOperationProblemBuilder problemBuilder) {
    return ops.runUnaryMap(name, this, problemBuilder);
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return ops.isSupportedBinary(name);
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runBinaryMap(name, this, argument, problemBuilder);
  }

  @Override
  public boolean isTernaryOpVectorized(String op) {
    return ops.isSupportedTernary(op);
  }

  @Override
  public Storage<?> runVectorizedTernaryMap(
      String name, Object argument0, Object argument1, MapOperationProblemBuilder problemBuilder) {
    return ops.runTernaryMap(name, this, argument0, argument1, problemBuilder);
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runZip(name, this, argument, problemBuilder);
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return NumericBuilder.createLongBuilder(capacity, getType());
  }

  @Override
  public abstract IntegerType getType();

  private static MapOperationStorage<Long, AbstractLongStorage> buildOps() {
    MapOperationStorage<Long, AbstractLongStorage> ops = new MapOperationStorage<>();
    ops.add(
            new LongNumericOp(Storage.Maps.ADD) {
              @Override
              public double doDouble(
                  long in, double arg, int ix, MapOperationProblemBuilder problemBuilder) {
                return in + arg;
              }

              @Override
              public Long doLong(
                  long in, long arg, int ix, MapOperationProblemBuilder problemBuilder) {
                try {
                  return Math.addExact(in, arg);
                } catch (ArithmeticException e) {
                  problemBuilder.reportOverflow(IntegerType.INT_64, in, "+", arg);
                  return null;
                }
              }
            })
        .add(
            new LongNumericOp(Storage.Maps.SUB) {
              @Override
              public double doDouble(
                  long in, double arg, int ix, MapOperationProblemBuilder problemBuilder) {
                return in - arg;
              }

              @Override
              public Long doLong(
                  long in, long arg, int ix, MapOperationProblemBuilder problemBuilder) {
                try {
                  return Math.subtractExact(in, arg);
                } catch (ArithmeticException e) {
                  problemBuilder.reportOverflow(IntegerType.INT_64, in, "-", arg);
                  return null;
                }
              }
            })
        .add(
            new LongNumericOp(Storage.Maps.MUL) {
              @Override
              public double doDouble(
                  long in, double arg, int ix, MapOperationProblemBuilder problemBuilder) {
                return in * arg;
              }

              @Override
              public Long doLong(
                  long in, long arg, int ix, MapOperationProblemBuilder problemBuilder) {
                try {
                  return Math.multiplyExact(in, arg);
                } catch (ArithmeticException e) {
                  problemBuilder.reportOverflow(IntegerType.INT_64, in, "*", arg);
                  return null;
                }
              }
            })
        .add(
            new LongNumericOp(Storage.Maps.MOD) {
              @Override
              public double doDouble(
                  long in, double arg, int ix, MapOperationProblemBuilder problemBuilder) {
                if (arg == 0.0) {
                  problemBuilder.reportDivisionByZero(ix);
                }
                return in % arg;
              }

              @Override
              public Long doLong(
                  long in, long arg, int ix, MapOperationProblemBuilder problemBuilder) {
                if (arg == 0) {
                  problemBuilder.reportDivisionByZero(ix);
                  return null;
                }

                return in % arg;
              }
            })
        .add(
            new LongNumericOp(Storage.Maps.POWER, true) {
              @Override
              public double doDouble(
                  long in, double arg, int ix, MapOperationProblemBuilder problemBuilder) {
                return Math.pow(in, arg);
              }

              @Override
              public Long doLong(
                  long in, long arg, int ix, MapOperationProblemBuilder problemBuilder) {
                throw new IllegalStateException(
                    "Internal error: Power operation should cast to double.");
              }
            })
        .add(
            new UnaryLongToLongOp(Maps.TRUNCATE) {
              @Override
              protected long doOperation(long a) {
                return a;
              }
            })
        .add(
            new UnaryLongToLongOp(Maps.CEIL) {
              @Override
              protected long doOperation(long a) {
                return a;
              }
            })
        .add(
            new UnaryLongToLongOp(Maps.FLOOR) {
              @Override
              protected long doOperation(long a) {
                return a;
              }
            })
        .add(new LongRoundOp(Maps.ROUND))
        .add(
            new LongNumericOp(Storage.Maps.DIV, true) {
              @Override
              public double doDouble(
                  long in, double arg, int ix, MapOperationProblemBuilder problemBuilder) {
                if (arg == 0.0) {
                  problemBuilder.reportDivisionByZero(ix);
                }
                return in / arg;
              }

              @Override
              public Long doLong(
                  long in, long arg, int ix, MapOperationProblemBuilder problemBuilder) {
                throw new UnsupportedOperationException("Divide operation should cast to double.");
              }
            })
        .add(
            new LongComparison(Storage.Maps.GT) {
              @Override
              protected boolean doLong(long a, long b) {
                return a > b;
              }

              @Override
              protected boolean doDouble(long a, double b) {
                return a > b;
              }
            })
        .add(
            new LongComparison(Storage.Maps.GTE) {
              @Override
              protected boolean doLong(long a, long b) {
                return a >= b;
              }

              @Override
              protected boolean doDouble(long a, double b) {
                return a >= b;
              }
            })
        .add(
            new LongComparison(Storage.Maps.LT) {
              @Override
              protected boolean doLong(long a, long b) {
                return a < b;
              }

              @Override
              protected boolean doDouble(long a, double b) {
                return a < b;
              }
            })
        .add(
            new LongComparison(Storage.Maps.LTE) {
              @Override
              protected boolean doLong(long a, long b) {
                return a <= b;
              }

              @Override
              protected boolean doDouble(long a, double b) {
                return a <= b;
              }
            })
        .add(
            new LongBooleanOp(Storage.Maps.EQ) {
              @Override
              public BoolStorage runBinaryMap(
                  AbstractLongStorage storage,
                  Object arg,
                  MapOperationProblemBuilder problemBuilder) {
                if (arg instanceof Double) {
                  problemBuilder.reportFloatingPointEquality(-1);
                }
                return super.runBinaryMap(storage, arg, problemBuilder);
              }

              @Override
              public BoolStorage runZip(
                  AbstractLongStorage storage,
                  Storage<?> arg,
                  MapOperationProblemBuilder problemBuilder) {
                if (arg instanceof DoubleStorage) {
                  problemBuilder.reportFloatingPointEquality(-1);
                } else if (!(arg instanceof AbstractLongStorage)) {
                  boolean hasFloats = false;
                  Context context = Context.getCurrent();
                  for (int i = 0; i < storage.size(); i++) {
                    if (arg.isNa(i)) {
                      continue;
                    }

                    if (arg.getItemBoxed(i) instanceof Double) {
                      hasFloats = true;
                      break;
                    }

                    context.safepoint();
                  }
                  if (hasFloats) {
                    problemBuilder.reportFloatingPointEquality(-1);
                  }
                }
                return super.runZip(storage, arg, problemBuilder);
              }

              @Override
              protected boolean doLong(long a, long b) {
                return a == b;
              }

              @Override
              protected boolean doDouble(long a, double b) {
                return a == b;
              }

              @Override
              protected boolean doObject(long x, Object o) {
                return false;
              }
            })
        .add(
            new UnaryMapOperation<>(Storage.Maps.IS_NOTHING) {
              @Override
              public BoolStorage runUnaryMap(
                  AbstractLongStorage storage, MapOperationProblemBuilder problemBuilder) {
                return new BoolStorage(storage.getIsMissing(), new BitSet(), storage.size(), false);
              }
            })
        .add(
            new UnaryMapOperation<>(Storage.Maps.IS_NAN) {
              @Override
              public BoolStorage runUnaryMap(
                  AbstractLongStorage storage, MapOperationProblemBuilder problemBuilder) {
                BitSet isNaN = new BitSet();
                return new BoolStorage(isNaN, storage.getIsMissing(), storage.size(), false);
              }
            })
        .add(
            new UnaryMapOperation<>(Storage.Maps.IS_INFINITE) {
              @Override
              public BoolStorage runUnaryMap(
                  AbstractLongStorage storage, MapOperationProblemBuilder problemBuilder) {
                BitSet isInfinite = new BitSet();
                return new BoolStorage(isInfinite, storage.getIsMissing(), storage.size(), false);
              }
            })
        .add(new LongIsInOp());
    return ops;
  }

  /**
   * Return an instance of storage containing the same data but with a wider type.
   *
   * <p>Ideally it should avoid copying the data, if it's possible.
   */
  public abstract AbstractLongStorage widen(IntegerType widerType);
}
