package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

/** A problem indicating that a quote has been opened and never closed. */
public record MismatchedQuote() implements Problem {}
