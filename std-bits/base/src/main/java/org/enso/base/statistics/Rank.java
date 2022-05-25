package org.enso.base.statistics;

import java.util.Arrays;
import java.util.Comparator;
import java.util.stream.IntStream;

public class Rank {
  public enum Method {
    AVERAGE,
    MINIMUM,
    MAXIMUM,
    DENSE,
    ORDINAL
  }

  private record ValueWithIndex(Object value, int index) {
  }

  public static double[] rank(Object[] input, Comparator<Object> comparator, Method method)
      throws NullPointerException, ClassCastException
  {
    Comparator<ValueWithIndex> tupleComparator = (a, b) -> {
      int c = comparator.compare(a.value, b.value);
      return c == 0 ? Integer.compare(a.index, b.index) : -c;
    };

    ValueWithIndex[] tuples = new ValueWithIndex[input.length];
    for(int i = 0; i < input.length; i++) {
      if (input[i] == null) {
        throw new NullPointerException("Value is Nothing at index " + i);
      }
      tuples[i] = new ValueWithIndex(input[i], i);
    }
    Arrays.sort(tuples, tupleComparator);

    double[] output = new double[input.length];

    int index = 0;
    int dense = 0;
    while (index < tuples.length)
    {
      dense++;
      int bottom = index;

      // Find Top
      while (index < tuples.length && comparator.compare(tuples[bottom].value,  tuples[index].value) == 0) {
        index++;
      }

      // Build Rank
      for (int i = bottom; i < index; i++) {
        double rank = switch (method) {
          case MINIMUM -> bottom + 1;
          case MAXIMUM -> index;
          case DENSE -> dense;
          case AVERAGE -> (bottom + 1 + index) / 2.0;
          case ORDINAL -> i + 1;
        };

        output[tuples[i].index] = rank;
      }
    }

    return output;
  }
}
