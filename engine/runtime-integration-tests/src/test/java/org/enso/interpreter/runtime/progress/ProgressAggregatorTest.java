package org.enso.interpreter.runtime.progress;

import static org.junit.Assert.assertEquals;

import java.util.function.Consumer;
import org.junit.Test;

public class ProgressAggregatorTest implements Consumer<Double> {
  private double current;

  public ProgressAggregatorTest() {}

  @Test
  public void threeSteps() {
    var agg = new ProgressAggregator(this);
    var three = agg.create(3);
    three.advance(1);
    assertEquals("1/3", 0.333, current, 0.001);
    three.advance(1);
    assertEquals("2/3", 0.666, current, 0.001);
    three.advance(1);
    assertEquals("3/3", 1.0, current, 0.001);
  }

  @Test
  public void closeFinishesItAll() {
    var agg = new ProgressAggregator(this);
    var ten = agg.create(10);
    ten.advance(1);
    assertEquals("1/10", 0.1, current, 0.001);
    ten.close();
    assertEquals("All done", 1.0, current, 0.001);
  }

  @Test
  public void halfAndThird() {
    var agg = new ProgressAggregator(this);
    var half = agg.create(2);
    var firstThird = agg.create(3);
    firstThird.advance(1);
    assertEquals("1/6", 1.0 / 6.0, current, 0.001);
    firstThird.advance(1);
    assertEquals("2/6", 0.333, current, 0.001);
    firstThird.close();
    assertEquals("3/6", 0.5, current, 0.001);
    half.advance(1);
    assertEquals("Topmost 1/2", 0.5, current, 0.001);

    var secondThird = agg.create(3);
    secondThird.advance(2);
    assertEquals("5/6", 5.0 / 6.0, current, 0.001);

    half.close();

    assertEquals("All done", 1.0, current, 0.001);
  }

  @Override
  public void accept(Double t) {
    this.current = t;
  }
}
