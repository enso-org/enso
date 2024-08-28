package org.enso.table.data.column.operation.map.numeric;

import java.math.BigDecimal;
import org.enso.base.numeric.Decimal_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

/** An operation rounding floating-point numbers. */
public class BigDecimalRoundOp
    extends TernaryMapOperation<BigDecimal, SpecializedStorage<BigDecimal>> {

  public BigDecimalRoundOp() {
    super(Storage.Maps.ROUND);
  }

  @Override
  public Storage<?> runTernaryMap(
      SpecializedStorage<BigDecimal> storage,
      Object decimalPlacesObject,
      Object useBankersObject,
      MapOperationProblemAggregator problemAggregator) {
    if (!(decimalPlacesObject instanceof Long decimalPlaces)) {
      throw new UnexpectedTypeException("a long.");
    }

    if (!(useBankersObject instanceof Boolean useBankers)) {
      throw new UnexpectedTypeException("a boolean.");
    }

    Builder builder =
        Builder.getForType(BigDecimalType.INSTANCE, storage.size(), problemAggregator);

    Context context = Context.getCurrent();

    for (int i = 0; i < storage.size(); i++) {
      if (!storage.isNothing(i)) {
        BigDecimal value = storage.getItem(i);
        BigDecimal result = Decimal_Utils.round(value, (int) decimalPlaces.longValue(), useBankers);
        builder.appendNoGrow(result);
      } else {
        builder.appendNulls(1);
      }
      context.safepoint();
    }

    return builder.seal();
  }
}
