package org.enso.table.parsing.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** A problem indicating that a row contained more or less columns than expected. */
public record InvalidRow(long sourceRow, Long tableIndex, String[] row, long expectedLength)
    implements Problem {
  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Invalid_Row",
        "Error",
        sourceRow(),
        tableIndex(),
        row(),
        expectedLength());
  }
}
