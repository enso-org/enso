package org.enso.base.statistics;

import org.enso.base.ObjectComparator;
import org.graalvm.polyglot.Context;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class Rank {
  private static final Comparator<Object> DOUBLE_COMPARATOR =  (a, b) -> Double.compare((Double)a, (Double)b);

  public enum Method {
    AVERAGE,
    MINIMUM,
    MAXIMUM,
    DENSE,
    ORDINAL
  }

  private record ValueWithIndex(Object value, int index) {
  }

  public static double[] rank(Object[] input, Method method)
      throws NullPointerException, ClassCastException
  {
    List<ValueWithIndex> tuples = new ArrayList<>(input.length);
    for(int i = 0; i < input.length; i++) {
      if (input[i] == null) {
        throw new NullPointerException("Value is Nothing at index " + i);
      }
      tuples.add(new ValueWithIndex(input[i], i));
    }

    return computeRankFromTuples(tuples, new ObjectComparator(), method);
  }

  public static double[][] pairedRanks(Double[] x, Double[] y, Method method)
      throws IllegalArgumentException, NullPointerException, ClassCastException
  {
    if (x.length != y.length) {
      throw new IllegalArgumentException("Left and right lengths are not the same.");
    }

    Context context = Context.getCurrent();
    List<ValueWithIndex> x_tuples = new ArrayList<>(x.length);
    List<ValueWithIndex> y_tuples = new ArrayList<>(y.length);
    for (int i = 0; i < x.length; i++) {
      if (x[i] == null || Double.isNaN(x[i]) || y[i] == null || Double.isNaN(y[i])) {
        continue;
      }

      x_tuples.add(new ValueWithIndex(x[i], x_tuples.size()));
      y_tuples.add(new ValueWithIndex(y[i], y_tuples.size()));

      context.safepoint();
    }

    return new double[][] {
        computeRankFromTuples(x_tuples, DOUBLE_COMPARATOR, method),
        computeRankFromTuples(y_tuples, DOUBLE_COMPARATOR, method)
    };
  }

  private static double[] computeRankFromTuples(List<ValueWithIndex> tuples, Comparator<Object> comparator, Method method)
      throws NullPointerException, ClassCastException
  {
    Comparator<ValueWithIndex> tupleComparator = (a, b) -> {
      int c = comparator.compare(a.value, b.value);
      return c == 0 ? Integer.compare(a.index, b.index) : -c;
    };
    tuples.sort(tupleComparator);

    double[] output = new double[tuples.size()];

    Context context = Context.getCurrent();
    int index = 0;
    int dense = 0;
    while (index < tuples.size()) {
      dense++;
      int start = index;

      // Find End of Equal Values
      while (index < tuples.size() && comparator.compare(tuples.get(start).value, tuples.get(index).value) == 0) {
        index++;
      }

      // Build Rank
      for (int i = start; i < index; i++) {
        double rank = switch (method) {
          case MINIMUM -> start + 1;
          case MAXIMUM -> index;
          case DENSE -> dense;
          case AVERAGE -> (start + 1 + index) / 2.0;
          case ORDINAL -> i + 1;
        };

        output[tuples.get(i).index] = rank;
      }

      context.safepoint();
    }

    return output;
  }
}
