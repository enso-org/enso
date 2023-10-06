package org.enso.table.data.column.operation.map;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.problems.ArithmeticError;
import org.enso.table.data.table.problems.ArithmeticOverflow;
import org.enso.table.data.table.problems.ColumnAggregatedProblem;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.data.table.problems.IllegalArgumentError;
import org.enso.table.problems.ProblemAggregator;

import java.util.ArrayList;
import java.util.List;

/**
 * This class is used to aggregate problems occurring during map operations performed on a storage.
 *
 * <p>A single instance of this builder should not be re-used for different map operations. It may
 * only be used with a single operation.
 */
public class MapOperationProblemBuilder extends ProblemAggregator {
  private final String location;
  private long overflowCount = 0;
  private Object[] overflowExample = null;
  private StorageType overflowTargetType = null;

  private final List<ColumnAggregatedProblem> aggregatedProblemList = new ArrayList<>();

  public MapOperationProblemBuilder(ProblemAggregator parent, String location) {
    super(parent);
    this.location = location;
  }

  protected void reportColumnAggregatedProblem(ColumnAggregatedProblem problem) {
    for (ColumnAggregatedProblem p : aggregatedProblemList) {
      if (p.merge(problem)) {
        // The problem was merged with an existing one.
        return;
      }
    }

    aggregatedProblemList.add(problem);
  }

  public void reportFloatingPointEquality(int row) {
    reportColumnAggregatedProblem(new FloatingPointGrouping(location, row));
  }

  public void reportArithmeticError(String message, Integer row) {
    reportColumnAggregatedProblem(new ArithmeticError(location, message, row));
  }

  public void reportIllegalArgumentError(String message, Integer row) {
    reportColumnAggregatedProblem(new IllegalArgumentError(location, message, row));
  }

  public void reportOverflow(StorageType targetType, long x, String op, long y) {
    overflowCount++;
    if (overflowTargetType == null) {
      overflowTargetType = targetType;
      overflowExample = new Object[]{x, op, y};
    }
  }

  public void reportDivisionByZero(Integer row) {
    reportArithmeticError("Division by zero", row);
  }

  @Override
  public ProblemSummary summarize() {
    var summary = super.summarize();
    if (overflowCount > 0) {
      summary.add(
          new ArithmeticOverflow(overflowTargetType, overflowCount, overflowExample));
    }

    for (var p : aggregatedProblemList) {
      summary.add(p);
    }

    return summary;
  }
}
