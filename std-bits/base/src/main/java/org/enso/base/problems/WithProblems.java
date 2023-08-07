package org.enso.base.problems;

import java.util.List;

public record WithProblems<ResultType, ProblemType, ProblemSetType extends ProblemSet<ProblemType>>(ResultType result,
                                                                                                    ProblemSetType problems) {
  public static <T, P> WithProblems<T, P, ProblemList<P>> fromList(T result, List<P> problems) {
    return new WithProblems<>(result, new ProblemList<>(problems));
  }
}
