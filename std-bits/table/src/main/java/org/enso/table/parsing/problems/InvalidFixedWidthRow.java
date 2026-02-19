package org.enso.table.parsing.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/**
 * A problem indicating that a line in a fixed-width file did not have enough characters to cover
 * the columns of a fixed-width layout.
 */
public record InvalidFixedWidthRow(
    long sourceLineNumber, Long tableRowNumber, long lineLength, long minimumLineLength)
    implements Problem {
  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Invalid_Fixed_Width_Row",
        "Error",
        sourceLineNumber(),
        tableRowNumber(),
        lineLength(),
        minimumLineLength());
  }
}
