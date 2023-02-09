package org.enso.table.data.column.operation.map.problems;

import org.graalvm.collections.EconomicSet;

public class OperationProblemBuilder {
    private final EconomicSet<MapOperationProblem> problems = EconomicSet.create();

    public void report(MapOperationProblem problem) {
        problems.add(problem);
    }

    public MapOperationProblem[] build() {
        return problems.toArray(new MapOperationProblem[0]);
    }
}
