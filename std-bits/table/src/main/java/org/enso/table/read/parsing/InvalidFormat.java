package org.enso.table.read.parsing;

import java.util.List;
import org.enso.table.read.ParsingProblem;

public record InvalidFormat(List<String> cells) implements ParsingProblem {}
