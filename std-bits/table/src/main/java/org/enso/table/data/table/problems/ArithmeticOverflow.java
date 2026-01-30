package org.enso.table.data.table.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** Indicates that an arithmetic operation did not fit in the target type. */
public record ArithmeticOverflow(
    char targetTypeChar, long targetTypeSize, long affectedRowCount, Object[] exampleOperands)
    implements Problem {

  @Override
  public Value asEnsoValue() {
    var valueType = null;
    var exampleOperandsVector = exampleOperands == null ? null : EnsoMeta.toEnsoArray(exampleOperands);
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Arithmetic_Overflow",
        "Warning",
        valueType,
        affectedRowCount,
        exampleOperandsVector);
  }

}
