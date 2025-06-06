package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.BigDecimalDivideOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.ModOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MulOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.PowerOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.SubOp;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BigDecimalType;

public final class BigDecimalStorage extends SpecializedStorage<BigDecimal>
    implements NumericFormattingStorage {
  private static final MapOperationStorage<BigDecimal, SpecializedStorage<BigDecimal>> OPS =
      buildOps();

  private static MapOperationStorage<BigDecimal, SpecializedStorage<BigDecimal>> buildOps() {
    MapOperationStorage<BigDecimal, SpecializedStorage<BigDecimal>> ops =
        new MapOperationStorage<>();
    return ops.add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new BigDecimalDivideOp<>())
        .add(new PowerOp<>())
        .add(new ModOp<>());
  }

  private final CachedPropertyCheck<Boolean> isNumericFormatRequired;

  /**
   * @param data the underlying data
   */
  public BigDecimalStorage(BigDecimal[] data) {
    super(BigDecimalType.INSTANCE, data);
    isNumericFormatRequired =
        new CachedPropertyCheck<>(() -> RequiresNumberFormatting.compute(this, null), false);
  }

  @Override
  protected Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return OPS.runBinaryMap(name, this, argument, problemAggregator);
  }

  @Override
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return OPS.runZip(name, this, argument, problemAggregator);
  }

  @Override
  protected SpecializedStorage<BigDecimal> newInstance(BigDecimal[] data) {
    return new BigDecimalStorage(data);
  }

  @Override
  protected BigDecimal[] newUnderlyingArray(int size) {
    return new BigDecimal[size];
  }

  /**
   * Checks if any numbers are large enough for the column to require formatin in the table viz.
   *
   * @return true/false if formatting is required
   */
  @Override
  public Boolean cachedNumericFormatCheck() throws InterruptedException {
    return isNumericFormatRequired.get();
  }
}
