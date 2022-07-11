package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

import java.util.List;

/** Indicates that a text value did not match the format expected of a datatype. */
public record InvalidFormat(String column, List<String> cells) implements Problem {}
