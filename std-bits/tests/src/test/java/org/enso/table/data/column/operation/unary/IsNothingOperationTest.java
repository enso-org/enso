package org.enso.table.data.column.operation.unary;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public class IsNothingOperationTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.createDefault();

  @Test
  public void checkPlainStorage() {
    var storage = Builder.getObjectBuilder(3).append("Hi").append(5).append(null).seal();
    var result = IsNothingOperation.INSTANCE.apply(storage, null);
    assertEquals(3, result.getSize());
    assertEquals(false, result.getItemBoxed(0));
    assertEquals(false, result.getItemBoxed(1));
    assertEquals(true, result.getItemBoxed(2));
  }

  @Test
  public void checkBoolStorage() {
    checkBoolStorage(false);
  }

  @Test
  public void checkBoolStorageNegated() {
    checkBoolStorage(true);
  }

  private void checkBoolStorage(boolean negate) {
    var storage = Builder.getForBoolean(3).append(true).append(false).appendNulls(1).seal();
    if (negate) {
      if (storage instanceof BoolStorage bs) {
        storage =
            new BoolStorage(bs.getValues(), bs.getValidityMap(), (int) bs.getSize(), true, null);
      } else {
        fail("Expecting bool storage: " + storage);
      }
    }
    var result = IsNothingOperation.INSTANCE.apply(storage, null);
    assertEquals(3, result.getSize());
    assertEquals(false, result.getItemBoxed(0));
    assertEquals(false, result.getItemBoxed(1));
    assertEquals(true, result.getItemBoxed(2));
  }
}
