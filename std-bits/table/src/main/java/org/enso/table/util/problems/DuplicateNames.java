package org.enso.table.util.problems;

import org.enso.table.problems.Problem;

public record DuplicateNames(String[] duplicatedNames) implements Problem {}
