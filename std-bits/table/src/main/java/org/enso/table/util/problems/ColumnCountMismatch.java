package org.enso.table.util.problems;

import org.enso.table.problems.Problem;

public record ColumnCountMismatch(int expected, int actual) implements Problem {
}

