package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import java.util.Objects;
import java.util.Random;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.test.utils.ContextUtils;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class LongStorageTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("enso", "arrow").assertGC(false).build();

  @BeforeClass
  public static void importAll() {
    ctx.eval("enso", "from Standard.Base import all");
  }

  @Test
  public void testCreateViaBuilderAndReadViaArrow() {
    var r = new Random();
    var s = r.nextLong(256);
    var b = Builder.getForLong(IntegerType.INT_64, s, null);
    for (var i = 0L; i < s; i++) {
      b.append(r.nextLong());
    }
    var storage = b.seal();
    assertNotEquals(0L, storage.rawAddress());

    var off = (storage.rawCapacity() / 8 + 1) / 8 * 8;
    var arr =
        ctx.eval("arrow", "cast[Int64]")
            .execute(storage.rawAddress() + off, storage.getSize(), storage.rawAddress());
    var sb = new StringBuilder();
    for (var i = 0L; i < s; i++) {
      var elem0 = storage.getItemBoxed(i);
      var value1 = arr.getArrayElement(i);
      var elem1 = value1.isNull() ? null : value1.asLong();
      if (!Objects.equals(elem0, elem1)) {
        sb.append("\n  at ").append(i).append(" ").append(elem0).append(" != ").append(elem1);
      }
    }
    assertEquals(sb.toString(), 0, sb.length());
  }
}
