package org.enso.table.parsing.problems;

/** A problem which indicates how many additional invalid rows were encountered. */
public record AdditionalInvalidRows(long count) implements ParsingProblem {}
