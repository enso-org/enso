package org.enso.table.data.column.operation.map.numeric.arithmetic;

import java.math.BigInteger;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.DoubleBuilder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;
import org.enso.table.data.column.storage.numeric.DoubleStorageFacade;
import org.enso.table.data.column.storage.type.FloatType;

public abstract class NumericBinaryOpReturningDouble<T extends Number, I extends Storage<? super T>>
    extends BinaryMapOperation<T, I> {

  public NumericBinaryOpReturningDouble(String name) {
    super(name);
  }

  private static ColumnDoubleStorage asDoubleStorage(Storage<?> storage) {
    return switch (storage) {
      case ColumnDoubleStorage s -> s;
      case BigDecimalStorage s -> DoubleStorageFacade.forBigDecimal(s);
      case BigIntegerStorage s -> DoubleStorageFacade.forBigInteger(s);
      default -> throw NumericBinaryOpImplementation.newUnsupported(storage);
    };
  }

  @Override
  public Storage<? extends Number> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return DoubleBuilder.makeEmpty(storage.getSize());
    }

    double rhs =
        (arg instanceof BigInteger bigInteger)
            ? bigInteger.doubleValue()
            : NumericConverter.coerceToDouble(arg);

    ColumnStorage<Double> result;
    if (storage instanceof ColumnLongStorage longStorage) {
      result =
          StorageIterators.buildOverLongStorage(
              longStorage,
              Builder.getForDouble(FloatType.FLOAT_64, longStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) ->
                  builder.appendDouble(doDouble(value, rhs, index, problemAggregator)));
    } else {
      var doubleStorage = asDoubleStorage(storage);
      result =
          StorageIterators.buildOverDoubleStorage(
              doubleStorage,
              Builder.getForDouble(FloatType.FLOAT_64, doubleStorage.getSize(), problemAggregator),
              (builder, index, value, isNothing) ->
                  builder.appendDouble(doDouble(value, rhs, index, problemAggregator)));
    }

    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  @Override
  public Storage<? extends Number> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    ColumnStorage<Double> result;
    if (storage instanceof ColumnLongStorage lhs) {
      if (arg instanceof ColumnLongStorage rhs) {
        result =
            StorageIterators.zipOverLongStorages(
                lhs,
                rhs,
                s -> Builder.getForDouble(FloatType.FLOAT_64, s, problemAggregator),
                true,
                (index, value1, isNothing1, value2, isNothing2) ->
                    doDouble(value1, value2, index, problemAggregator));
      } else {
        result =
            StorageIterators.zipOverLongDoubleStorages(
                lhs,
                asDoubleStorage(arg),
                s -> Builder.getForDouble(FloatType.FLOAT_64, s, problemAggregator),
                true,
                (index, value1, isNothing1, value2, isNothing2) ->
                    doDouble(value1, value2, index, problemAggregator));
      }
    } else if (arg instanceof ColumnLongStorage rhs) {
      result =
          StorageIterators.zipOverDoubleLongStorages(
              asDoubleStorage(storage),
              rhs,
              s -> Builder.getForDouble(FloatType.FLOAT_64, s, problemAggregator),
              true,
              (index, value1, isNothing1, value2, isNothing2) ->
                  doDouble(value1, value2, index, problemAggregator));
    } else {
      result =
          StorageIterators.zipOverDoubleStorages(
              asDoubleStorage(storage),
              asDoubleStorage(arg),
              s -> Builder.getForDouble(FloatType.FLOAT_64, s, problemAggregator),
              true,
              (index, value1, isNothing1, value2, isNothing2) ->
                  doDouble(value1, value2, index, problemAggregator));
    }

    // ToDo: Merge Storage and ColumnStorage
    return (Storage<Double>) result;
  }

  protected abstract double doDouble(
      double a, double b, long ix, MapOperationProblemAggregator problemAggregator);
}
