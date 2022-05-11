package org.enso.table.read;

import java.util.List;

public record WithWarnings<T>(T value, List<ParsingProblem> problems) {}
