package org.enso.table.parsing.problems;

import java.util.List;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** Indicates that a text value did not match the format expected of a datatype. */
public record InvalidFormat(
    String column, Value expectedEnsoValueType, long count, List<String> cells) implements Problem {
  @Override
  public Value asEnsoValue() {
    var vectorType = EnsoMeta.getType("Standard.Base.Data.Vector", "Vector");
    var cellsVector = vectorType.invokeMember("from_polyglot_array", cells());

    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Invalid_Format",
        "Error",
        column(),
        expectedEnsoValueType(),
        count(),
        cellsVector);
  }
}
