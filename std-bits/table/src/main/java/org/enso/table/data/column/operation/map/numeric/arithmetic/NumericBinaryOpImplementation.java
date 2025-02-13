package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.BitSet;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorageFacade;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.error.UnexpectedTypeException;

/** An operation expecting a numeric argument and returning a numeric column. */
public abstract class NumericBinaryOpImplementation<T extends Number, I extends Storage<? super T>>
    extends BinaryMapOperation<T, I> {

  protected abstract double doDouble(double a, double b, long ix, MapOperationProblemAggregator problemAggregator);

  protected abstract Long doLong(long a, long b, long ix, MapOperationProblemAggregator problemAggregator);

  protected abstract BigInteger doBigInteger(BigInteger a, BigInteger b, long ix, MapOperationProblemAggregator problemAggregator);

  protected abstract BigDecimal doBigDecimal(BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator);

  static IllegalStateException newUnsupported(Object arg) {
    return new IllegalStateException("Unsupported storage: " + arg.getClass().getCanonicalName());
  }

  // The type to use for small integer results (regardless of the input bit size).
  public static final IntegerType INTEGER_RESULT_TYPE = IntegerType.INT_64;

  public NumericBinaryOpImplementation(String name) {
    super(name);
  }

  @Override
  public Storage<?> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return allNullStorageOfSameType(storage);
    }

    if (arg instanceof BigInteger rhs) {
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(s, new BigDecimal(rhs), problemAggregator);
        case BigIntegerStorage s -> runBigIntegerMap(s, rhs, problemAggregator);
        case ColumnDoubleStorage s -> runDoubleMap(s, rhs.doubleValue(), problemAggregator);
        case ColumnLongStorage s -> runBigIntegerLongMap(s, rhs, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else if (NumericConverter.isCoercibleToLong(arg)) {
      long argAsLong = NumericConverter.coerceToLong(arg);
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(s, BigDecimal.valueOf(argAsLong), problemAggregator);
        case BigIntegerStorage s -> runBigIntegerMap(s, BigInteger.valueOf(argAsLong), problemAggregator);
        case ColumnDoubleStorage s -> runDoubleMap(s, (double) argAsLong, problemAggregator);
        case ColumnLongStorage s -> runLongMap(s, argAsLong, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else if (NumericConverter.isCoercibleToDouble(arg)) {
      double argAsDouble = NumericConverter.coerceToDouble(arg);
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(s, BigDecimal.valueOf(argAsDouble), problemAggregator);
        case BigIntegerStorage s ->
            runDoubleMap(new DoubleStorageFacade<>(s, BigInteger::doubleValue), argAsDouble, problemAggregator);
        case ColumnDoubleStorage s -> runDoubleMap(s, argAsDouble, problemAggregator);
        case ColumnLongStorage s -> runDoubleLongMap(s, argAsDouble, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else if (arg instanceof BigDecimal bd) {
      return switch (storage) {
        case BigDecimalStorage s -> runBigDecimalMap(s, bd, problemAggregator);
        case BigIntegerStorage s ->
            runBigDecimalMap(new ColumnStorageFacade<>(s, BigDecimal::new), bd, problemAggregator);
        case ColumnDoubleStorage s ->
            runBigDecimalMap(new ColumnStorageFacade<>(s, BigDecimal::valueOf), bd, problemAggregator);
        case ColumnLongStorage s ->
            runBigDecimalMap(new ColumnStorageFacade<>(s, BigDecimal::valueOf), bd, problemAggregator);
        default -> throw newUnsupported(storage);
      };
    } else {
      throw new UnexpectedTypeException("a Number.");
    }
  }

  @Override
  public Storage<? extends Number> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    if (storage instanceof ColumnDoubleStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs ->
            runBigDecimalZip(new ColumnStorageFacade<>(lhs, BigDecimal::valueOf), rhs, problemAggregator);
        case BigIntegerStorage rhs ->
            runDoubleZip(lhs, new DoubleStorageFacade<>(rhs, BigInteger::doubleValue), problemAggregator);
        case ColumnDoubleStorage rhs -> runDoubleZip(lhs, rhs, problemAggregator);
        case ColumnLongStorage rhs ->
            runDoubleZip(lhs, new DoubleStorageFacade<>(rhs, Long::doubleValue), problemAggregator);
        default -> throw newUnsupported(arg);
      };
    } else if (storage instanceof ColumnLongStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs ->
            runBigDecimalZip(new ColumnStorageFacade<>(lhs, BigDecimal::valueOf), rhs, problemAggregator);
        case BigIntegerStorage rhs ->
            runBigIntegerZip(new ColumnStorageFacade<>(lhs, BigInteger::valueOf), rhs, problemAggregator);
        case ColumnDoubleStorage rhs ->
            runDoubleZip(new DoubleStorageFacade<>(lhs, Long::doubleValue), rhs, problemAggregator);
        case ColumnLongStorage rhs -> runLongZip(lhs, rhs, problemAggregator);
        default -> throw newUnsupported(arg);
      };
    } else if (storage instanceof BigIntegerStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs ->
            runBigDecimalZip(new ColumnStorageFacade<>(lhs, BigDecimal::new), rhs, problemAggregator);
        case BigIntegerStorage rhs -> runBigIntegerZip(lhs, rhs, problemAggregator);
        case ColumnDoubleStorage rhs ->
            runDoubleZip(new DoubleStorageFacade<>(lhs, BigInteger::doubleValue), rhs, problemAggregator);
        case ColumnLongStorage rhs ->
            runBigIntegerZip(lhs, new ColumnStorageFacade<>(rhs, BigInteger::valueOf), problemAggregator);
        default -> throw newUnsupported(arg);
      };
    } else if (storage instanceof BigDecimalStorage lhs) {
      return switch (arg) {
        case BigDecimalStorage rhs -> runBigDecimalZip(lhs, rhs, problemAggregator);
        case BigIntegerStorage rhs ->
            runBigDecimalZip(lhs, new ColumnStorageFacade<>(rhs, BigDecimal::new), problemAggregator);
        case ColumnDoubleStorage rhs ->
            runBigDecimalZip(lhs, new ColumnStorageFacade<>(rhs, BigDecimal::valueOf), problemAggregator);
        case ColumnLongStorage rhs ->
            runBigDecimalZip(lhs, new ColumnStorageFacade<>(rhs, BigDecimal::valueOf), problemAggregator);
        default -> throw newUnsupported(arg);
      };
    } else {
      throw newUnsupported(storage);
    }
  }

  private static Storage<? extends Number> allNullStorageOfSameType(Storage<?> storage) {
    return switch (storage) {
      case ColumnLongStorage s -> LongStorage.makeEmpty(storage.getSize(), INTEGER_RESULT_TYPE);
      case BigIntegerStorage s -> BigIntegerStorage.makeEmpty(storage.getSize());
      case BigDecimalStorage s -> BigDecimalStorage.makeEmpty(storage.getSize());
      case ColumnDoubleStorage s -> DoubleStorage.makeEmpty(storage.getSize());
      default -> throw new IllegalStateException(
          "Unsupported storage: " + storage.getClass().getCanonicalName());
    };
  }

  protected Storage<Double> runDoubleZip(
      ColumnDoubleStorage a, ColumnDoubleStorage b, MapOperationProblemAggregator problemAggregator) {
    var result = StorageIterators.zipOverDoubleStorages(
        a,
        b,
        s-> Builder.getForDouble(FloatType.FLOAT_64, s, problemAggregator),
        true,
        (index, value1, isNothing1, value2, isNothing2) -> doDouble(value1, value2, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  protected Storage<Double> runDoubleLongMap(
      ColumnLongStorage a, Double b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            Builder.getForDouble(FloatType.FLOAT_64, a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(doDouble((double) value, b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  protected Storage<Double> runDoubleMap(
      ColumnDoubleStorage a, Double b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverDoubleStorage(
            a,
            Builder.getForDouble(FloatType.FLOAT_64, a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(doDouble(value, b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  protected Storage<Long> runLongZip(
      ColumnLongStorage a,
      ColumnLongStorage b,
      MapOperationProblemAggregator problemAggregator) {
    var result = StorageIterators.zipOverLongStorages(
        a,
        b,
        s-> Builder.getForLong(INTEGER_RESULT_TYPE, s, problemAggregator),
        true,
        (index, value1, isNothing1, value2, isNothing2) -> doLong(value1, value2, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Long>) result;
  }

  protected Storage<Long> runLongMap(
      ColumnLongStorage a, Long b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            Builder.getForLong(INTEGER_RESULT_TYPE, a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(doLong(value, b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Long>) result;
  }

  protected Storage<BigInteger> runBigIntegerZip(
      ColumnStorage<BigInteger> a,
      ColumnStorage<BigInteger> b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            a,
            b,
            s -> Builder.getForBigInteger(s, problemAggregator),
            true,
            (index, x, y) -> doBigInteger(x, y, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigInteger>) result;
  }

  protected Storage<BigInteger> runBigIntegerLongMap(
      ColumnLongStorage a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            Builder.getForBigInteger(a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(
                    doBigInteger(BigInteger.valueOf(value), b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigInteger>) result;
  }

  protected Storage<BigInteger> runBigIntegerMap(
      ColumnStorage<BigInteger> a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.mapOverStorage(
            a,
            Builder.getForBigInteger(a.getSize(), problemAggregator),
            (index, value) -> doBigInteger(value, b, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigInteger>) result;
  }

  protected Storage<BigDecimal> runBigDecimalZip(
      ColumnStorage<BigDecimal> a,
      ColumnStorage<BigDecimal> b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            a,
            b,
            Builder::getForBigDecimal,
            true,
            (index, x, y) -> doBigDecimal(x, y, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigDecimal>) result;
  }

  protected Storage<BigDecimal> runBigDecimalMap(
      ColumnStorage<BigDecimal> a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.mapOverStorage(
            a,
            Builder.getForBigDecimal(a.getSize()),
            (index, value) -> doBigDecimal(value, b, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigDecimal>) result;
  }
}
