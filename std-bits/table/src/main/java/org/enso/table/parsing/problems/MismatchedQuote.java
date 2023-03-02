package org.enso.table.parsing.problems;

/**
 * Indicates that a Delimited file is corrupted because it contains a quote that was opened and
 * never closed.
 */
public class MismatchedQuote extends RuntimeException {}
