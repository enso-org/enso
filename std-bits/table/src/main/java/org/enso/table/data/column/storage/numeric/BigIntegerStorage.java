package org.enso.table.data.column.storage.numeric;

import org.enso.table.data.column.builder.BigIntegerBuilder;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.numeric.arithmetic.AddOp;
import org.enso.table.data.column.operation.map.numeric.arithmetic.DivideOp;
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
import org.enso.table.data.column.storage.type.BigIntegerType;
import org.enso.table.data.column.storage.type.StorageType;

import java.math.BigInteger;

public class BigIntegerStorage extends SpecializedStorage<BigInteger> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public BigIntegerStorage(BigInteger[] data, int size) {
    super(data, size, makeOps());
  }

  protected static MapOperationStorage<BigInteger, SpecializedStorage<BigInteger>> makeOps() {
    MapOperationStorage<BigInteger, SpecializedStorage<BigInteger>> ops = ObjectStorage.buildObjectOps();
    return ops
        .add(new AddOp<>())
        .add(new SubOp<>())
        .add(new MulOp<>())
        .add(new DivideOp<>())
        .add(new ModOp<>())
        .add(new PowerOp<>())
        .add(new LessComparison<>())
        .add(new LessOrEqualComparison<>())
        .add(new EqualsComparison<>())
        .add(new GreaterOrEqualComparison<>())
        .add(new GreaterComparison<>());
  }

  public static BigIntegerStorage makeEmpty(int size) {
    return new BigIntegerStorage(new BigInteger[size], size);
  }

  @Override
  protected SpecializedStorage<BigInteger> newInstance(BigInteger[] data, int size) {
    return new BigIntegerStorage(data, size);
  }

  @Override
  protected BigInteger[] newUnderlyingArray(int size) {
    return new BigInteger[0];
  }

  @Override
  public StorageType getType() {
    return BigIntegerType.INSTANCE;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new BigIntegerBuilder(capacity);
  }
}
