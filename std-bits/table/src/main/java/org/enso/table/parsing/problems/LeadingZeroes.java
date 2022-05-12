package org.enso.table.parsing.problems;

import java.util.List;

public record LeadingZeroes(List<String> cells) implements ParsingProblem {}
