package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** Indicates that an arithmetic operation did not fit in the target type. */
public record ArithmeticOverflow(
    StorageType<?> targetType, long affectedRowCount, Object[] exampleOperands) implements Problem {
  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Arithmetic_Overflow",
        "Warning",
        targetType().asEnsoValueType(),
        affectedRowCount(),
        exampleOperands());
  }
}
