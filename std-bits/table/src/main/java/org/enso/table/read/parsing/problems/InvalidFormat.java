package org.enso.table.read.parsing.problems;

import java.util.List;

public record InvalidFormat(List<String> cells) implements ParsingProblem {}
