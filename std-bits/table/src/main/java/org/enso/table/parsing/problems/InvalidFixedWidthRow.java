package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

/** A problem indicating that a row contained more or less columns than expected. */
public record InvalidFixedWidthRow(long source_line_number, Long table_row_number, long line_length, long minimum_line_length)
    implements Problem {}
