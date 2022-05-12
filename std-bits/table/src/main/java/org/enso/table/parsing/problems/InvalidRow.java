package org.enso.table.parsing.problems;

/** A problem indicating that a row contained more or less columns than expected. */
public record InvalidRow(long source_row, Long table_index, String[] row) implements ParsingProblem {}
