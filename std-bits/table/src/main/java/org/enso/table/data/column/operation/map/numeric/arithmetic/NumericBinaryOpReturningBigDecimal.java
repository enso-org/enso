package org.enso.table.data.column.operation.map.numeric.arithmetic;

import static org.enso.table.data.column.operation.map.numeric.arithmetic.NumericBinaryOpImplementation.asBigDecimal;

import java.math.BigDecimal;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.storage.*;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.numeric.BigIntegerStorage;

public abstract class NumericBinaryOpReturningBigDecimal<
        T extends Number, I extends Storage<? super T>>
    extends BinaryMapOperation<T, I> {
  public NumericBinaryOpReturningBigDecimal(String name) {
    super(name);
  }

  private static ColumnStorage<BigDecimal> asBigDecimalStorage(Storage<?> storage) {
    return switch (storage) {
      case ColumnDoubleStorage s -> asBigDecimal(s);
      case ColumnLongStorage s -> asBigDecimal(s);
      case BigDecimalStorage s -> s;
      case BigIntegerStorage s -> asBigDecimal(s);
      default -> throw NumericBinaryOpImplementation.newUnsupported(storage);
    };
  }

  @Override
  public Storage<BigDecimal> runBinaryMap(
      I storage, Object arg, MapOperationProblemAggregator problemAggregator) {
    if (arg == null) {
      return BigDecimalStorage.makeEmpty(storage.getSize());
    }

    var lhs = asBigDecimalStorage(storage);
    BigDecimal rhs = NumericConverter.coerceToBigDecimal(arg);
    var result =
        StorageIterators.mapOverStorage(
            lhs,
            Builder.getForBigDecimal(lhs.getSize()),
            (index, value) -> doBigDecimal(value, rhs, index, problemAggregator));
    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigDecimal>) result;
  }

  @Override
  public Storage<BigDecimal> runZip(
      I storage, Storage<?> arg, MapOperationProblemAggregator problemAggregator) {
    var lhs = asBigDecimalStorage(storage);
    var rhs = asBigDecimalStorage(arg);

    var result =
        StorageIterators.zipOverStorages(
            lhs,
            rhs,
            s -> Builder.getForBigDecimal(s),
            true,
            (index, value1, value2) -> doBigDecimal(value1, value2, index, problemAggregator));

    // ToDo: Merge Storage and ColumnStorage
    return (Storage<BigDecimal>) result;
  }

  public abstract BigDecimal doBigDecimal(
      BigDecimal a, BigDecimal b, long ix, MapOperationProblemAggregator problemAggregator);
}
