package org.enso.table.data.table.problems;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.Problem;

/** Indicates that an arithmetic operation did not fit in the target type. */
public record ArithmeticOverflow(
    StorageType targetType,
    long affectedRowCount,
    Object[] exampleOperands
) implements Problem {
}
