package org.enso.table.data.column.operation.map;

import org.enso.table.data.table.problems.ArithmeticError;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.problems.AggregatedProblems;

public class MapOperationProblemBuilder {
  private final String location;
  private final AggregatedProblems problems = new AggregatedProblems(3);

  public MapOperationProblemBuilder(String location) {
    this.location = location;
  }

  public AggregatedProblems getProblems() {
    return problems;
  }

  public void reportFloatingPointEquality(int row) {
    problems.add(new FloatingPointGrouping(location, row));
  }

  public void reportArithmeticError(String message, Integer row) {
    problems.add(new ArithmeticError(location, message, row));
  }

  public void reportDivisionByZero(Integer row) {
    reportArithmeticError("Division by zero", row);
  }
}
