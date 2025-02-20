package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.FloatType;

/**
 * A variant of NumericBinaryOpImplementation that has different null behaviour: if one of the
 * values is null, the other non-null value is returned.
 */
public abstract class NumericBinaryOpCoalescing<T extends Number, I extends Storage<? super T>>
    extends NumericBinaryOpImplementation<T, I> {
  public NumericBinaryOpCoalescing(String name) {
    super(name);
  }

  @Override
  public Storage<?> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return storage;
    }
    return super.runBinaryMap(storage, arg, problemAggregator);
  }

  @Override
  protected Storage<Double> runDoubleZip(
      ColumnDoubleStorage a,
      ColumnDoubleStorage b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverDoubleStorages(
            a,
            b,
            s -> Builder.getForDouble(FloatType.FLOAT_64, s, problemAggregator),
            false,
            (index, value1, isNothing1, value2, isNothing2) -> {
              if (isNothing1 && isNothing2) {
                return null;
              } else if (isNothing1) {
                return value2;
              } else if (isNothing2) {
                return value1;
              } else {
                return doDouble(value1, value2, index, problemAggregator);
              }
            });
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  @Override
  protected Storage<Double> runDoubleLongMap(
      ColumnLongStorage a, Double b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            false,
            Builder.getForDouble(FloatType.FLOAT_64, a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(
                    isNothing ? b : doDouble((double) value, b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  @Override
  protected Storage<Double> runDoubleMap(
      ColumnDoubleStorage a, Double b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverDoubleStorage(
            a,
            true,
            Builder.getForDouble(FloatType.FLOAT_64, a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(isNothing ? b : doDouble(value, b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  @Override
  protected Storage<Long> runLongZip(
      ColumnLongStorage a, ColumnLongStorage b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverLongStorages(
            a,
            b,
            s -> Builder.getForLong(INTEGER_RESULT_TYPE, s, problemAggregator),
            false,
            (index, value1, isNothing1, value2, isNothing2) -> {
              if (isNothing1 && isNothing2) {
                return null;
              } else if (isNothing1) {
                return value2;
              } else if (isNothing2) {
                return value1;
              } else {
                return doLong(value1, value2, index, problemAggregator);
              }
            });
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Long>) result;
  }

  @Override
  protected Storage<Long> runLongMap(
      ColumnLongStorage a, Long b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            false,
            Builder.getForLong(INTEGER_RESULT_TYPE, a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(isNothing ? b : doLong(value, b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Long>) result;
  }

  @Override
  protected Storage<BigInteger> runBigIntegerZip(
      ColumnStorage<BigInteger> a,
      ColumnStorage<BigInteger> b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            a,
            b,
            s -> Builder.getForBigInteger(s, problemAggregator),
            false,
            (index, x, y) -> {
              if (x == null && y == null) {
                return null;
              } else if (x == null) {
                return y;
              } else if (y == null) {
                return x;
              } else {
                return doBigInteger(x, y, index, problemAggregator);
              }
            });
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigInteger>) result;
  }

  @Override
  protected Storage<BigInteger> runBigIntegerLongMap(
      ColumnLongStorage a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.buildOverLongStorage(
            a,
            false,
            Builder.getForBigInteger(a.getSize(), problemAggregator),
            (builder, index, value, isNothing) ->
                builder.append(
                    isNothing
                        ? b
                        : doBigInteger(BigInteger.valueOf(value), b, index, problemAggregator)));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigInteger>) result;
  }

  @Override
  protected Storage<BigInteger> runBigIntegerMap(
      ColumnStorage<BigInteger> a, BigInteger b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.mapOverStorage(
            a,
            false,
            Builder.getForBigInteger(a.getSize(), problemAggregator),
            (index, value) -> value == null ? b : doBigInteger(value, b, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigInteger>) result;
  }

  @Override
  protected Storage<BigDecimal> runBigDecimalZip(
      ColumnStorage<BigDecimal> a,
      ColumnStorage<BigDecimal> b,
      MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.zipOverStorages(
            a,
            b,
            s -> Builder.getForBigDecimal(s),
            false,
            (index, x, y) -> {
              if (x == null && y == null) {
                return null;
              } else if (x == null) {
                return y;
              } else if (y == null) {
                return x;
              } else {
                return doBigDecimal(x, y, index, problemAggregator);
              }
            });
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigDecimal>) result;
  }

  @Override
  protected Storage<BigDecimal> runBigDecimalMap(
      ColumnStorage<BigDecimal> a, BigDecimal b, MapOperationProblemAggregator problemAggregator) {
    var result =
        StorageIterators.mapOverStorage(
            a,
            false,
            Builder.getForBigDecimal(a.getSize()),
            (index, value) -> value == null ? b : doBigDecimal(value, b, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigDecimal>) result;
  }
}
