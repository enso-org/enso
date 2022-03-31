package org.enso.table.data.table.aggregate;

import org.enso.table.aggregations.*;
import org.enso.table.data.table.Column;

public class AggregateColumnDefinition {
  public static GroupBy GroupBy(String name, Column column) {
    return new GroupBy(name, column);
  }

  public static Count Count(String name) {
    return new Count(name);
  }

  public static CountNothing CountNothing(String name, Column column, boolean isNothing) {
    return new CountNothing(name, column, isNothing);
  }

  public static CountEmpty CountEmpty(String name, Column column, boolean isEmpty) {
    return new CountEmpty(name, column, isEmpty);
  }

  public static CountDistinct CountDistinct(String name, Column[] columns, boolean ignoreEmpty) {
    return new CountDistinct(name, columns, ignoreEmpty);
  }

  public static MinOrMax MinOrMax(String name, Column column, int minOrMax) {
    return new MinOrMax(name, column, minOrMax);
  }

  public static MinOrMaxLength MinOrMaxLength(String name, Column column, int minOrMax) {
    return new MinOrMaxLength(name, column, minOrMax);
  }

  public static Sum Sum(String name, Column column) {
    return new Sum(name, column);
  }

  public static Mean Mean(String name, Column column) {
    return new Mean(name, column);
  }

  public static StandardDeviation StDev(String name, Column column, boolean population) {
    return new StandardDeviation(name, column, population);
  }

  public static First First(String name, Column column, boolean ignoreNothing) {
    return new First(name, column, ignoreNothing);
  }

  public static Last Last(String name, Column column, boolean ignoreNothing) {
    return new Last(name, column, ignoreNothing);
  }

  public static Concatenate Concatenate(String name, Column column, String join, String prefix, String suffix, String quote) {
    return new Concatenate(name, column, join, prefix, suffix, quote);
  }

  public static Mode Mode(String name, Column column) {
    return new Mode(name, column);
  }

  public static Percentile Percentile(String name, Column column, double percentile) {
    return new Percentile(name, column, percentile);
  }
}
