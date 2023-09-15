package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.ArithmeticError;
import org.enso.table.data.table.problems.ArithmeticOverflow;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.data.table.problems.IllegalArgumentError;
import org.enso.table.problems.AggregatedProblems;

/**
 * This class is used to aggregate problems occurring during map operations performed on a storage.
 *
 * <p>A single instance of this builder should not be re-used for different map operations. It may
 * only be used with a single operation.
 */
public class MapOperationProblemBuilder {
  private final String location;
  private final AggregatedProblems problems = new AggregatedProblems(10);
  private long overflowCount = 0;
  private Object[] overflowExample = null;
  private StorageType overflowTargetType = null;

  public MapOperationProblemBuilder(String location) {
    this.location = location;
  }

  public AggregatedProblems getProblems() {
    AggregatedProblems additionalProblems = new AggregatedProblems();
    if (overflowCount > 0) {
      additionalProblems.add(
          new ArithmeticOverflow(overflowTargetType, overflowCount, overflowExample));
    }
    return AggregatedProblems.merge(problems, additionalProblems);
  }

  public void reportFloatingPointEquality(int row) {
    problems.add(new FloatingPointGrouping(location, row));
  }

  public void reportArithmeticError(String message, Integer row) {
    problems.add(new ArithmeticError(location, message, row));
  }

  public void reportIllegalArgumentError(String message, Integer row) {
    problems.add(new IllegalArgumentError(location, message, row));
  }

  public void reportOverflow(StorageType targetType, long x, String op, long y) {
    overflowCount++;
    if (overflowTargetType == null) {
      overflowTargetType = targetType;
      overflowExample = new Object[] {x, op, y};
    }
  }

  public void reportDivisionByZero(Integer row) {
    reportArithmeticError("Division by zero", row);
  }
}
