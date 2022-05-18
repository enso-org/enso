package org.enso.table.parsing.problems;

/** A problem indicating that a quote has been opened and never closed. */
public record MismatchedQuote() implements ParsingProblem {}
