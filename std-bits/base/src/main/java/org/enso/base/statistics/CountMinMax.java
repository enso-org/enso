package org.enso.base.statistics;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.function.BiFunction;
import java.util.stream.Stream;

public class CountMinMax {
  private static boolean isValid(Object v) {
    return !(v == null || (v instanceof Double && Double.isNaN((Double)v)));
  }

  public static Stream<Object> toObjectStream(Object[] array) {
    return Arrays.stream(array);
  }

  public final int count;
  public final boolean comparatorError;
  public final Object minimum;
  public final Object maximum;

  public CountMinMax(Stream<Object> values, Comparator<Object> objectComparator) {
    int count = 0;

    boolean min_max_failed = false;
    Object minimum = null;
    Object maximum = null;

    Iterator<Object> iterator = values.filter(CountMinMax::isValid).iterator();
    while (iterator.hasNext()) {
      Object value = iterator.next();
      count++;

      if (!min_max_failed) {
        try {
          minimum = minimum == null || objectComparator.compare(minimum, value) > 0 ? value : minimum;
          maximum = maximum == null || objectComparator.compare(maximum, value) < 0 ? value : maximum;
        } catch (ClassCastException e) {
          min_max_failed = true;
        }
      }
    }

    this.count = count;
    this.comparatorError = min_max_failed;
    this.minimum = min_max_failed ? null : minimum;
    this.maximum = min_max_failed ? null : maximum;
  }
}
