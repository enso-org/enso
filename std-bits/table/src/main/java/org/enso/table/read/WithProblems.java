package org.enso.table.read;

import java.util.List;
import org.enso.table.parsing.problems.ParsingProblem;

public record WithProblems<T>(T value, List<ParsingProblem> problems) {}
