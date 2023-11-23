package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

import java.util.List;

/**
 * Indicates that a text value did not match the format expected of a datatype.
 */
public record InvalidFormat(String column, Value expectedEnsoValueType, List<String> cells) implements Problem {
}
