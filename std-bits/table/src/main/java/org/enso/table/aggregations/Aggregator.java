package org.enso.table.aggregations;

import org.enso.table.data.table.problems.AggregatedProblems;
import org.enso.table.data.table.problems.Problem;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/***
 * Interface used to define aggregate columns.
 */
public abstract class Aggregator {
  private final String name;
  private final int type;
  private AggregatedProblems problems;

  protected Aggregator(String name, int type) {
    this.name = name;
    this.type = type;
    this.problems = null;
  }

  /***
   * @return Name of the new column.
   */
  public final String getName() {
    return name;
  }

  /***
   * @return The type of the new column.
   */
  public int getType() {
    return type;
  }

  public AggregatedProblems getProblems() {
    return problems;
  }

  /***
   * Compute the value for a set of rows
   * @param indexes - indexes to the rows in the source table to aggregate on
   * @return aggregated value
   */
  public Object aggregate(int[] indexes) {
    return this.aggregate(Arrays.stream(indexes).boxed().collect(Collectors.toList()));
  }

  /***
   * Compute the value for a set of rows
   * @param indexes - indexes to the rows in the source table to aggregate on
   * @return aggregated value
   */
  public abstract Object aggregate(List<Integer> indexes);

  protected void addProblem(Problem problem) {
    if (problems == null) {
      problems = new AggregatedProblems();
    }
    problems.add(problem);
  }

  protected static Long CastToLong(Object value) {
    if (value instanceof Long) {
      return (Long)value;
    } else if (value instanceof Integer) {
      return ((Integer)value).longValue();
    } else if (value instanceof Byte) {
      return ((Byte)value).longValue();
    } else if (value instanceof Float && ((Float)value) % 1 == 0) {
      // Only return if an integer stored as a float ( % 1 == 0)
      return ((Float)value).longValue();
    } else if (value instanceof Double && ((Double)value) % 1 == 0) {
      // Only return if an integer stored as a double ( % 1 == 0)
      return ((Double)value).longValue();
    }

    return null;
  }

  protected static Double CastToDouble(Object value) {
    if (value instanceof Long) {
      return ((Long)value).doubleValue();
    } else if (value instanceof Integer) {
      return ((Integer)value).doubleValue();
    } else if (value instanceof Byte) {
      return ((Byte)value).doubleValue();
    } else if (value instanceof Float) {
      return ((Float)value).doubleValue();
    } else if (value instanceof Double) {
      return ((Double)value);
    }

    return null;
  }
}
