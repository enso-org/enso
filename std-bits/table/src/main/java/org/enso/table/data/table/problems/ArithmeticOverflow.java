package org.enso.table.data.table.problems;

import org.enso.table.problems.Problem;

/** Indicates that an arithmetic operation did not fit in the target type. */
public record ArithmeticOverflow(
    char targetTypeChar, long targetTypeSize, long affectedRowCount, Object[] exampleOperands)
    implements Problem {}
