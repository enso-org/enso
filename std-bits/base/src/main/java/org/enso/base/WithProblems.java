package org.enso.base;

import java.util.List;

public record WithProblems<ResultType, ProblemType>(ResultType result, List<ProblemType> problems) {}
