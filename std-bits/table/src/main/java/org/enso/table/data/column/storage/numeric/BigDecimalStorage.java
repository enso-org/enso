package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import org.enso.table.data.column.operation.CachedPropertyCheck;
import org.enso.table.data.column.operation.RequiresNumberFormatting;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.BigDecimalDivideOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.ModOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MulOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.PowerOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.SubOp;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;

public final class BigDecimalStorage extends SpecializedStorage<BigDecimal>
    implements NumericFormattingStorage {

  private CachedPropertyCheck<Boolean> isNumericFormatRequired;

  /**
   * @param data the underlying data
   */
  public BigDecimalStorage(BigDecimal[] data) {
    super(BigDecimalType.INSTANCE, data, buildOps());
    isNumericFormatRequired =
        new CachedPropertyCheck<>(() -> RequiresNumberFormatting.compute(this, null), false);
  }

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
