package org.enso.base.statistics;

import org.graalvm.polyglot.Value;

public class CountMinMax {
  public long count;
  public boolean comparatorError;
  public Value minimum;
  public Value maximum;

  public CountMinMax() {
    this.count = 0;
    this.comparatorError = false;
    this.minimum = Value.asValue(null);
    this.maximum = Value.asValue(null);
  }

  public void increment() {
    this.count++;
  }

  public void failComparator() {
    this.comparatorError = true;
    this.minimum = Value.asValue(null);
    this.maximum = Value.asValue(null);
  }

  public void setMinimum(Value value) {
    this.minimum = value;
  }

  public void setMaximum(Value value) {
    this.maximum = value;
  }
}
