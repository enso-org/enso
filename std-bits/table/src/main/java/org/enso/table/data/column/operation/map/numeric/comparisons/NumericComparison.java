package org.enso.table.data.column.operation.map.numeric.comparisons;

import static org.enso.table.data.column.operation.map.numeric.arithmetic.NumericBinaryOpImplementation.asBigDecimal;
import static org.enso.table.data.column.operation.map.numeric.arithmetic.NumericBinaryOpImplementation.asBigInteger;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.base.CompareException;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorageFacade;

public abstract class NumericComparison<T extends Number, I extends Storage<? super T>>
    extends BinaryMapOperation<T, I> {

  protected abstract boolean doDouble(double a, double b);

  protected abstract boolean doLong(long a, long b);

  protected abstract boolean doBigInteger(BigInteger a, BigInteger b);

  protected abstract boolean doBigDecimal(BigDecimal a, BigDecimal b);

  protected boolean onOtherType(Object a, Object b) {
    throw new CompareException(a, b);
  }

  static IllegalStateException newUnsupported(Object arg) {
    return new IllegalStateException("Unsupported storage: " + arg.getClass().getCanonicalName());
  }

  public NumericComparison(String name) {
    super(name);
  }

  @Override
  public Storage<Boolean> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BoolStorage.makeEmpty(storage.getSize());
    }

    if (arg instanceof BigInteger bigInteger) {
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(
            s, new BigDecimal(bigInteger), problemAggregator);
        case BigIntegerStorage s -> runBigIntegerMap(s, bigInteger, problemAggregator);
        case ColumnDoubleStorage s -> runDoubleMap(s, bigInteger.doubleValue(), problemAggregator);
        case ColumnLongStorage s -> runBigIntegerLongMap(s, bigInteger, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else if (arg instanceof BigDecimal bigDecimal) {
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(s, bigDecimal, problemAggregator);
        case BigIntegerStorage s -> runBigDecimalMap(
            asBigDecimal(s), bigDecimal, problemAggregator);
        case ColumnDoubleStorage s -> runBigDecimalMap(
            asBigDecimal(s), bigDecimal, problemAggregator);
        case ColumnLongStorage s -> runBigDecimalMap(
            asBigDecimal(s), bigDecimal, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else if (NumericConverter.isCoercibleToLong(arg)) {
      long argAsLong = NumericConverter.coerceToLong(arg);
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(
            s, BigDecimal.valueOf(argAsLong), problemAggregator);
        case BigIntegerStorage s -> runBigIntegerMap(
            s, BigInteger.valueOf(argAsLong), problemAggregator);
        case ColumnDoubleStorage s -> runDoubleMap(s, (double) argAsLong, problemAggregator);
        case ColumnLongStorage s -> runLongMap(s, argAsLong, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else if (NumericConverter.isCoercibleToDouble(arg)) {
      double argAsDouble = NumericConverter.coerceToDouble(arg);
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(
            s, BigDecimal.valueOf(argAsDouble), problemAggregator);
        case BigIntegerStorage s -> runDoubleMap(
            DoubleStorageFacade.forBigInteger(s), argAsDouble, problemAggregator);
        case ColumnDoubleStorage s -> runDoubleMap(s, argAsDouble, problemAggregator);
        case ColumnLongStorage s -> runDoubleLongMap(s, argAsDouble, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else {
      var result =
          StorageIterators.buildOverStorage(
              (Storage<?>) storage,
              Builder.getForBoolean(storage.getSize()),
              (builder, index, value) -> builder.appendBoolean(onOtherType(value, arg)));
      return (Storage<Boolean>) result;
    }
  }

  protected Storage<Boolean> runLongMap(
      ColumnLongStorage a, long b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            Builder.getForBoolean(a.getSize()),
            (builder, index, value, isNothing) -> builder.appendBoolean(doLong(value, b)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runDoubleLongMap(
      ColumnLongStorage a, Double b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            Builder.getForBoolean(a.getSize()),
            (builder, index, value, isNothing) -> builder.append(doDouble((double) value, b)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runDoubleMap(
      ColumnDoubleStorage a, double b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverDoubleStorage(
            a,
            Builder.getForBoolean(a.getSize()),
            (builder, index, value, isNothing) -> builder.appendBoolean(doDouble(value, b)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runBigIntegerLongMap(
      ColumnLongStorage a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            Builder.getForBoolean(a.getSize()),
            (builder, index, value, isNothing) ->
                builder.appendBoolean(doBigInteger(BigInteger.valueOf(value), b)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runBigIntegerMap(
      ColumnStorage<BigInteger> a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverStorage(
            a,
            Builder.getForBoolean(a.getSize()),
            (builder, index, value) -> builder.appendBoolean(doBigInteger(value, b)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runBigDecimalMap(
      ColumnStorage<BigDecimal> a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverStorage(
            a,
            Builder.getForBoolean(a.getSize()),
            (builder, index, value) -> builder.appendBoolean(doBigDecimal(value, b)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  @Override
  public Storage<Boolean> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnDoubleStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs -> runBigDecimalZip(asBigDecimal(lhs), rhs, problemAggregator);
        case BigIntegerStorage rhs -> runDoubleZip(
            lhs, DoubleStorageFacade.forBigInteger(rhs), problemAggregator);
        case ColumnDoubleStorage rhs -> runDoubleZip(lhs, rhs, problemAggregator);
        case ColumnLongStorage rhs -> runDoubleLongZip(lhs, rhs, problemAggregator);
        default -> runMixedZip(storage, arg, problemAggregator);
      };
    } else if (storage instanceof ColumnLongStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs -> runBigDecimalZip(asBigDecimal(lhs), rhs, problemAggregator);
        case BigIntegerStorage rhs -> runBigIntegerZip(asBigInteger(lhs), rhs, problemAggregator);
        case ColumnDoubleStorage rhs -> runLongDoubleZip(lhs, rhs, problemAggregator);
        case ColumnLongStorage rhs -> runLongZip(lhs, rhs, problemAggregator);
        default -> runMixedZip(storage, arg, problemAggregator);
      };
    } else if (storage instanceof BigIntegerStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs -> runBigDecimalZip(asBigDecimal(lhs), rhs, problemAggregator);
        case BigIntegerStorage rhs -> runBigIntegerZip(lhs, rhs, problemAggregator);
        case ColumnDoubleStorage rhs -> runDoubleZip(
            DoubleStorageFacade.forBigInteger(lhs), rhs, problemAggregator);
        case ColumnLongStorage rhs -> runBigIntegerZip(lhs, asBigInteger(rhs), problemAggregator);
        default -> runMixedZip(storage, arg, problemAggregator);
      };
    } else if (storage instanceof BigDecimalStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs -> runBigDecimalZip(lhs, rhs, problemAggregator);
        case BigIntegerStorage rhs -> runBigDecimalZip(lhs, asBigDecimal(rhs), problemAggregator);
        case ColumnDoubleStorage rhs -> runBigDecimalZip(lhs, asBigDecimal(rhs), problemAggregator);
        case ColumnLongStorage rhs -> runBigDecimalZip(lhs, asBigDecimal(rhs), problemAggregator);
        default -> runMixedZip(storage, arg, problemAggregator);
      };
    } else {
      throw newUnsupported(storage);
    }
  }

  protected Storage<Boolean> runLongZip(
      ColumnLongStorage a, ColumnLongStorage b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverLongStorages(
            a,
            b,
            Builder::getForBoolean,
            true,
            (index, x, xIsNothing, y, yIsNothing) -> doLong(x, y));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runLongDoubleZip(
      ColumnLongStorage a, ColumnDoubleStorage b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverLongDoubleStorages(
            a,
            b,
            Builder::getForBoolean,
            true,
            (index, x, xIsNothing, y, yIsNothing) -> doDouble(x, y));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runDoubleZip(
      ColumnDoubleStorage a,
      ColumnDoubleStorage b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverDoubleStorages(
            a,
            b,
            Builder::getForBoolean,
            true,
            (index, x, xIsNothing, y, yIsNothing) -> doDouble(x, y));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runDoubleLongZip(
      ColumnDoubleStorage a, ColumnLongStorage b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverDoubleLongStorages(
            a,
            b,
            Builder::getForBoolean,
            true,
            (index, x, xIsNothing, y, yIsNothing) -> doDouble(x, y));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runBigIntegerZip(
      ColumnStorage<BigInteger> a,
      ColumnStorage<BigInteger> b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            a, b, Builder::getForBoolean, true, (index, x, y) -> doBigInteger(x, y));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runBigDecimalZip(
      ColumnStorage<BigDecimal> a,
      ColumnStorage<BigDecimal> b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            a, b, Builder::getForBoolean, true, (index, x, y) -> doBigDecimal(x, y));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }

  protected Storage<Boolean> runMixedZip(
      Storage<?> lhs, Storage<?> rhs, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            lhs,
            rhs,
            Builder::getForBoolean,
            true,
            (index, x, y) -> {
              boolean r;
              // Any number is coercible to double, if the value is not coercible, it is not a
              // supported
              // number type.
              if (NumericConverter.isCoercibleToDouble(x)
                  && NumericConverter.isCoercibleToDouble(y)) {
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
              return r;
            });
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Boolean>) result;
  }
}
