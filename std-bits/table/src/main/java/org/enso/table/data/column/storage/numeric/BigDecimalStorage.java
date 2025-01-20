package org.enso.table.data.column.storage.numeric;

import java.math.BigDecimal;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.numeric.BigDecimalRoundOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.BigDecimalDivideOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MaxOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MinOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.ModOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.MulOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.PowerOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.SubOp;
import org.enso.table.data.column.operation.map.numeric.comparisons.EqualsComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.GreaterOrEqualComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessComparison;
import org.enso.table.data.column.operation.map.numeric.comparisons.LessOrEqualComparison;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.StorageType;

public final class BigDecimalStorage extends SpecializedStorage<BigDecimal> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public BigDecimalStorage(BigDecimal[] data, int size) {
    super(data, size, buildOps());
  }

  public static BigDecimalStorage makeEmpty(int size) {
    return new BigDecimalStorage(new BigDecimal[size], size);
  }

  private static MapOperationStorage<BigDecimal, SpecializedStorage<BigDecimal>> buildOps() {
    MapOperationStorage<BigDecimal, SpecializedStorage<BigDecimal>> ops =
        ObjectStorage.buildObjectOps();
    return ops.add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new BigDecimalDivideOp<>())
        .add(new BigDecimalRoundOp())
        .add(new PowerOp<>())
        .add(new ModOp<>())
        .add(new MinOp<>())
        .add(new MaxOp<>())
        .add(new LessComparison<>())
        .add(new LessOrEqualComparison<>())
        .add(new EqualsComparison<>())
        .add(new GreaterOrEqualComparison<>())
        .add(new GreaterComparison<>());
  }

  @Override
  protected SpecializedStorage<BigDecimal> newInstance(BigDecimal[] data, int size) {
    return new BigDecimalStorage(data, size);
  }

  @Override
  protected BigDecimal[] newUnderlyingArray(int size) {
    return new BigDecimal[size];
  }

  @Override
  public StorageType getType() {
    return BigDecimalType.INSTANCE;
  }
}
