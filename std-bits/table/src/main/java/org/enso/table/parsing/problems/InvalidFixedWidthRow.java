package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

/**
 * A problem indicating that a line in a fixed-width file did not have enough characters to cover
 * the columns of a fixed-width layout.
 */
public record InvalidFixedWidthRow(
    long source_line_number, Long table_row_number, long line_length, long minimum_line_length)
    implements Problem {}
