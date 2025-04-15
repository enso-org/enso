package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

/** A problem indicating that a fixed-width file had lines of different lengths. */
public record InconsistentFixedWidthLengths() implements Problem {}
