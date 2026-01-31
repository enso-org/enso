package org.enso.table.util.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

public record InvalidNames(String[] invalidNames) implements Problem {
  @Override
  public Value asEnsoValue() {
    return EnsoMeta.makeInstance(
        "Standard.Table.Errors", "Invalid_Column_Names", "Error", invalidNames(), null);
  }
}
