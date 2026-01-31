package org.enso.table.util.problems;

import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

public record DuplicateNames(String[] duplicatedNames) implements Problem {
  @Override
  public Value asEnsoValue() {
    var vectorType = EnsoMeta.getType("Standard.Base.Data.Vector", "Vector");
    var namesVector = vectorType.invokeMember("from_polyglot_array", (Object) duplicatedNames());

    return EnsoMeta.makeInstance(
        "Standard.Table.Errors", "Duplicate_Output_Column_Names", "Error", namesVector);
  }
}
