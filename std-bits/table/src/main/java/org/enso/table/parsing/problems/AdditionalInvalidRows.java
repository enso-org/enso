package org.enso.table.parsing.problems;

import org.enso.table.problems.Problem;

/** A problem which indicates how many additional invalid rows were encountered. */
public record AdditionalInvalidRows(long count) implements Problem {}
