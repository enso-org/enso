package org.enso.interpreter.test;

import org.enso.interpreter.runtime.error.Warning;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.error.WithWarnings;
import org.junit.Assert;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class WarningsTest {
  @Test
  public void doubleWithWarningsWrap() {
    var warn1 = new Warning("w1", this, 1L);
    var warn2 = new Warning("w2", this, 2L);
    var value = 42;

    var with1 = WithWarnings.wrap(42, warn1);
    var with2 = WithWarnings.wrap(with1, warn2);

    assertEquals(value, with1.getValue());
    assertEquals(value, with2.getValue());
    Assert.assertArrayEquals(
        new Object[] {warn1}, with1.getWarningsArray(WarningsLibrary.getUncached()));
    Assert.assertArrayEquals(
        new Object[] {warn1, warn2}, with2.getWarningsArray(WarningsLibrary.getUncached()));
  }
}
