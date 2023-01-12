package org.enso.table.data.table.join;

import org.enso.table.problems.AggregatedProblems;
import org.graalvm.collections.Pair;

import java.util.List;

public record JoinResult(List<Pair<Integer, Integer>> matchedRows, AggregatedProblems problems) {}
