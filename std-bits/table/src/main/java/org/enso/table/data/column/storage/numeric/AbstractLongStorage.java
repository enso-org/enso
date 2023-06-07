package org.enso.table.data.column.storage.numeric;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.numeric.LongBooleanOp;
import org.enso.table.data.column.operation.map.numeric.LongIsInOp;
import org.enso.table.data.column.operation.map.numeric.LongNumericOp;
import org.enso.table.data.column.operation.map.UnaryLongToLongOp;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.BitSet;

public abstract class AbstractLongStorage extends NumericStorage<Long> {
  public abstract long getItem(int idx);

  public abstract BitSet getIsMissing();

  @Override
  public double getItemDouble(int idx) {
    return (double) getItem(idx);
  }

  private static final MapOpStorage<Long, AbstractLongStorage> ops = buildOps();

  @Override
  public boolean isOpVectorized(String name) {
    return ops.isSupported(name);
  }

  @Override
  protected Storage<?> runVectorizedMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runMap(name, this, argument, problemBuilder);
  }

  @Override
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return ops.runZip(name, this, argument, problemBuilder);
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return NumericBuilder.createLongBuilder(capacity);
  }

  private static MapOpStorage<Long, AbstractLongStorage> buildOps() {
    MapOpStorage<Long, AbstractLongStorage> ops = new MapOpStorage<>();
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
                return in + arg;
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
                return in - arg;
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
                return in * arg;
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
            new LongBooleanOp(Storage.Maps.GT) {
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
            new LongBooleanOp(Storage.Maps.GTE) {
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
            new LongBooleanOp(Storage.Maps.LT) {
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
            new LongBooleanOp(Storage.Maps.LTE) {
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
              public BoolStorage runMap(
                  AbstractLongStorage storage,
                  Object arg,
                  MapOperationProblemBuilder problemBuilder) {
                if (arg instanceof Double) {
                  problemBuilder.reportFloatingPointEquality(-1);
                }
                return super.runMap(storage, arg, problemBuilder);
              }

              @Override
              public BoolStorage runZip(
                  AbstractLongStorage storage,
                  Storage<?> arg,
                  MapOperationProblemBuilder problemBuilder) {
                if (arg instanceof DoubleStorage) {
                  problemBuilder.reportFloatingPointEquality(-1);
                } else if (!(arg instanceof LongStorage)) {
                  boolean hasFloats = false;
                  for (int i = 0; i < storage.size(); i++) {
                    if (arg.isNa(i)) {
                      continue;
                    }

                    if (arg.getItemBoxed(i) instanceof Double) {
                      hasFloats = true;
                      break;
                    }
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
              public BoolStorage run(AbstractLongStorage storage) {
                return new BoolStorage(storage.getIsMissing(), new BitSet(), storage.size(), false);
              }
            })
        .add(new LongIsInOp());
    return ops;
  }
}
