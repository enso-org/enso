package org.enso.table.data.column.operation.unary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.enso.table.data.column.builder.Builder;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public class CountNothingTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void checkPlainStorageOne() {
    var storage = Builder.getObjectBuilder(3).append("Hi").append(5).append(null).seal();
    assertEquals(1, CountNothing.apply(storage));
    assertTrue(CountNothing.anyNothing(storage));
    assertFalse(CountNothing.allNothing(storage));
  }

  @Test
  public void checkBoolStorageOne() {
    var storage = Builder.getForBoolean(3).append(true).append(false).appendNulls(1).seal();
    assertEquals(1, CountNothing.apply(storage));
    assertTrue(CountNothing.anyNothing(storage));
    assertFalse(CountNothing.allNothing(storage));
  }

  @Test
  public void checkPlainStorageNone() {
    var storage = Builder.getObjectBuilder(3).append("Hi").append(5).append(3.14).seal();
    assertEquals(0, CountNothing.apply(storage));
    assertFalse(CountNothing.anyNothing(storage));
    assertFalse(CountNothing.allNothing(storage));
  }

  @Test
  public void checkBoolStorageNone() {
    var storage = Builder.getForBoolean(3).append(true).append(false).append(false).seal();
    assertEquals(0, CountNothing.apply(storage));
    assertFalse(CountNothing.anyNothing(storage));
    assertFalse(CountNothing.allNothing(storage));
  }

  @Test
  public void checkPlainStorageAll() {
    var storage = Builder.getObjectBuilder(3).appendNulls(3).seal();
    assertEquals(3, CountNothing.apply(storage));
    assertTrue(CountNothing.anyNothing(storage));
    assertTrue(CountNothing.allNothing(storage));
  }

  @Test
  public void checkBoolStorageAll() {
    var storage = Builder.getForBoolean(3).appendNulls(3).seal();
    assertEquals(3, CountNothing.apply(storage));
    assertTrue(CountNothing.anyNothing(storage));
    assertTrue(CountNothing.allNothing(storage));
  }
}
