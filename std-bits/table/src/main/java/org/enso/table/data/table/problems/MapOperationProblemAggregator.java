package org.enso.table.data.table.problems;

import java.util.HashSet;
import java.util.Set;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.ColumnAggregatedProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

/**
 * This class is used to aggregate problems occurring during map operations performed on a storage.
 *
 * <p>A single instance of this aggregator should not be re-used for different map operations. It
 * may only be used with a single operation.
 */
public class MapOperationProblemAggregator extends ColumnAggregatedProblemAggregator {
  private final String location;

  private long overflowCount = 0;
  private Object[] overflowExample = null;
  private StorageType<?> overflowTargetType = null;

  private long invalidJSONCount = 0;
  private Set<String> invalidJSONExamples = new HashSet<>();

  public MapOperationProblemAggregator(ProblemAggregator parent, String location) {
    super(parent);
    this.location = location;
  }

  public void reportFloatingPointEquality(long row) {
    reportColumnAggregatedProblem(new FloatingPointGrouping(location, row));
  }

  public void reportArithmeticError(String message, long row) {
    reportColumnAggregatedProblem(new ArithmeticError(location, message, row));
  }

  public void reportIllegalArgumentError(String message, long row) {
    reportColumnAggregatedProblem(new IllegalArgumentError(location, message, row));
  }

  public void reportInvalidJSONError(String json) {
    invalidJSONCount++;
    if (invalidJSONExamples.size() < 10) {
      invalidJSONExamples.add(json);
    }
  }

  public void reportOverflow(StorageType<?> targetType, long x, String op, long y) {
    overflowCount++;
    if (overflowTargetType == null) {
      overflowTargetType = targetType;
      overflowExample = new Object[] {x, op, y};
    }
  }

  public void reportDivisionByZero(long row) {
    reportArithmeticError("Division by zero", row);
  }

  @Override
  public ProblemSummary summarize() {
    var summary = super.summarize();

    if (overflowCount > 0) {
      summary.add(new ArithmeticOverflow(overflowTargetType, overflowCount, overflowExample));
    }

    if (invalidJSONCount > 0) {
      summary.add(
          new InvalidJSONError(invalidJSONCount, invalidJSONExamples.toArray(new String[0])));
    }

    return summary;
  }
}
