package org.enso.table.read.parsing.problems;

import java.util.List;

public record LeadingZeroes(List<String> cells) implements ParsingProblem {}
