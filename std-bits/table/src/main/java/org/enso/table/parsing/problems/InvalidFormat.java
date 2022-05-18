package org.enso.table.parsing.problems;

import java.util.List;

/** Indicates that a text value did not match the format expected of a datatype. */
public record InvalidFormat(List<String> cells) implements ParsingProblem {}
