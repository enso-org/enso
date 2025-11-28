package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;

import java.util.Objects;
import java.util.Random;
import java.util.stream.LongStream;
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
  public void testCreateViaBuilderAndReadViaArrowSimple16() {
    generateAndCompare("Simple 16 values", 16, LongStream.range(0, 16));
  }

  @Test
  public void testCreateViaBuilderAndReadViaArrowRandom() {
    generateAndCompareWithSeed(System.currentTimeMillis());
  }

  private void generateAndCompareWithSeed(long seed) {
    var r = new Random(seed);
    var size = r.nextInt(256, 4096);
    var stream = r.longs(size);
    var msg = "with seed " + seed + " size " + size;
    System.err.println(msg);
    generateAndCompare(msg, size, stream);
  }

  private void generateAndCompare(String info, int size, LongStream r) {
    var sb = new StringBuilder();
    var b = Builder.getForLong(IntegerType.INT_64, size, null);
    r.forEach(b::append);
    var storage = b.seal();
    assertEquals("Storage has the right size: " + storage, size, storage.getSize());
    assertNotEquals(0L, storage.rawAddress());

    var arr = ctx.eval("arrow", "cast[Int64]").execute(storage.rawAddress(), storage.rawCapacity());
    for (var i = 0L; i < size; i++) {
      var elem0 = storage.getItemBoxed(i);
      var value1 = arr.getArrayElement(i);
      var elem1 = value1.isNull() ? null : value1.asLong();
      if (!Objects.equals(elem0, elem1)) {
        sb.append("\n  at ").append(i).append(" ").append(elem0).append(" != ").append(elem1);
      }
    }
    assertEquals(info + "\n" + sb.toString(), 0, sb.length());
  }
}
