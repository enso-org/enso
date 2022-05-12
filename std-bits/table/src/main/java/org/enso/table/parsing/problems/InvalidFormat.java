package org.enso.table.parsing.problems;

import java.util.List;

public record InvalidFormat(List<String> cells) implements ParsingProblem {}
