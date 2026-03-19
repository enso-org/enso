package org.enso.table.data.table.problems;

import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

/** Indicates that an parse JSON operation did not succeed. */
public record InvalidJSONError(long affectedRowCount, String[] exampleJSON) implements Problem {
  @Override
  public Value asEnsoValue() {
    var examples =
        Arrays.stream(exampleJSON).map(json -> "\"" + json + "\"").collect(Collectors.joining());
    var message =
        "There are " + affectedRowCount + " invalid JSON rows suchs as [" + examples + "]";
    return EnsoMeta.makeInstance("Standard.Base.Data.Json", "Invalid_JSON", "Error", message);
  }
}
