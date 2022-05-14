package org.enso.table.parsing.problems;

import java.util.List;

public record LeadingZeros(List<String> cells) implements ParsingProblem {}
