package org.enso.table.parsing.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** A problem which indicates how many additional invalid rows were encountered. */
public record AdditionalInvalidRows(long count) implements Problem {
  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors", "Additional_Invalid_Rows", "Error", count());
  }
}
