package org.enso.table.util.problems;

import org.enso.table.problems.Problem;

public record InvalidNames(String[] invalidNames) implements Problem {}
