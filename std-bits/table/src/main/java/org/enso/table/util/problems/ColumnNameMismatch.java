package org.enso.table.util.problems;

import org.enso.table.problems.Problem;

public record ColumnNameMismatch(String[] missingNames, String[] extraNames) implements Problem {
}
